import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, id, src, style, title, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mouse exposing (onDown, onMove, onUp)
import Navigation
import Task


-- Constants
scaledWidthPx : Int
scaledWidthPx = 960


-- Model
type alias CsrfToken =
  { tokenName : String
  , tokenValue : String
  }
type alias FileItem =
  { name : String
  , dir : Bool
  }
type alias FileItems =
  { path : List String
  , fileItems : List FileItem
  }
type alias Point =
  { xPixel : Int
  , yPixel : Int
  }
type alias Dimension =
  { widthPixel : Int
  , heightPixel : Int
  }
type alias Rectangle =
  { location : Point
  , size : Dimension
  }
-- This makes it very rigid, but easy to enforce
type ShapeId = Face | Eye1 | Eye2
type Shapes
  = FaceOnly Rectangle
  | FaceAndOneEye Rectangle Rectangle
  | FaceAndTwoEyes Rectangle Rectangle Rectangle
type alias MousePos = (Float, Float)
type MouseDragState
  = Moving (Rectangle -> Shapes -> Shapes) Rectangle MousePos
  | Resizing (Rectangle -> Shapes -> Shapes) Rectangle Dimension Dimension MousePos
type alias WorkingAnnotation =
  { imageSize : Dimension
  , scaleDown : Int -> Int
  , scaleUp : Int -> Int
  , shapes : Maybe Shapes
  , modified : Bool
  , mouseDragState : Maybe MouseDragState
  }
type alias Model =
  { csrfToken: CsrfToken
  , path : List String
  , fileItems : List FileItem
  , workingAnnotation : Maybe WorkingAnnotation
  , message: Maybe (Result String String)
  }
type alias Annotation =
  { label : String
  , shape : Rectangle
  }
type alias Annotations =
  { annotations : List Annotation
  }
type alias Metadata =
  { size : Dimension
  }


-- Message
type Msg
  = NewLocation Navigation.Location
  -- List images
  | NewFileItems (Result Http.Error FileItems)
  -- Edit annotation page load
  | NewAnnotation (Result Http.Error Annotations)
  | NewMetadata (Result Http.Error Metadata)
  -- Edit annotation mouse events
  | DragToCreateBoxStart Mouse.Event
  | DragToResizeBoxStart ShapeId MousePos
  | DragToResizeBoxMove MousePos
  | DragToMoveBoxStart ShapeId MousePos
  | DragToMoveBoxMove MousePos
  | DragStop
  -- Edit annotation save/cancel events
  | SubmitAnnotationRequest
  | SubmitAnnotationResponse (Result Http.Error Annotations)


-- JSON
fileItemDecoder : Decode.Decoder FileItem
fileItemDecoder =
  Decode.map2 FileItem
  (Decode.field "name" Decode.string)
  (Decode.field "dir" Decode.bool)


fileItemsDecoder : Decode.Decoder FileItems
fileItemsDecoder =
  Decode.map2 FileItems
  (Decode.field "path" (Decode.list Decode.string))
  (Decode.field "fileItems" (Decode.list fileItemDecoder))


pointDecoder : Decode.Decoder Point
pointDecoder =
  Decode.map2 Point
  (Decode.field "leftPixel" Decode.int)
  (Decode.field "topPixel" Decode.int)


dimensionDecoder : Decode.Decoder Dimension
dimensionDecoder =
  Decode.map2 Dimension
  (Decode.field "widthPixel" Decode.int)
  (Decode.field "heightPixel" Decode.int)


-- TODO make rectangle model consistent from server to JSON to client (JSON is a bit weird, maybe)
rectangleDecoder : Decode.Decoder Rectangle
rectangleDecoder =
  Decode.map2 Rectangle
  pointDecoder
  dimensionDecoder


annotationDecoder : Decode.Decoder Annotation
annotationDecoder =
  Decode.map2 Annotation
  (Decode.field "label" Decode.string)
  (Decode.field "shape" rectangleDecoder)


annotationsDecoder : Decode.Decoder Annotations
annotationsDecoder =
  Decode.map Annotations
  (Decode.field "annotations" (Decode.list annotationDecoder))


metadataDecoder : Decode.Decoder Metadata
metadataDecoder =
  Decode.map Metadata
  (Decode.field "size" dimensionDecoder)


init : CsrfToken -> Navigation.Location -> (Model, Cmd Msg)
init csrfToken location =
  ( Model csrfToken [] [] Nothing Nothing
  , Task.perform NewLocation (Task.succeed location)
  )


pathSpec : List String -> String
pathSpec path =
  String.concat (List.map ((++) "/" << Http.encodeUri) path)


relativize : Rectangle -> Rectangle -> Rectangle
relativize container content =
  let
    containerLoc : Point
    containerLoc = container.location

    contentLoc : Point
    contentLoc = content.location
  in
    { content
    | location =
      Point
      (contentLoc.xPixel - containerLoc.xPixel)
      (contentLoc.yPixel - containerLoc.yPixel)
    }


absolutize : Rectangle -> Rectangle -> Rectangle
absolutize container content =
  let
    containerLoc : Point
    containerLoc = container.location

    contentLoc : Point
    contentLoc = content.location
  in
    { content
    | location =
      Point
      (contentLoc.xPixel + containerLoc.xPixel)
      (contentLoc.yPixel + containerLoc.yPixel)
    }


getShape : ShapeId -> Shapes -> Maybe Rectangle
getShape shapeId shapes =
  case (shapeId, shapes) of
    (Face, FaceOnly faceRect) -> Just faceRect
    (Face, FaceAndOneEye faceRect _) -> Just faceRect
    (Face, FaceAndTwoEyes faceRect _ _) -> Just faceRect
    (Eye1, FaceAndOneEye _ eye1Rect) -> Just eye1Rect
    (Eye1, FaceAndTwoEyes _ eye1Rect _) -> Just eye1Rect
    (Eye2, FaceAndTwoEyes _ _ eye2Rect) -> Just eye2Rect
    _ -> Nothing


updateShapeInShapes : ShapeId -> Rectangle -> Shapes -> Shapes
updateShapeInShapes shapeId updatedRect shapes =
  case (shapeId, shapes) of
    (Face, FaceOnly _) -> FaceOnly updatedRect
    (Face, FaceAndOneEye _ eye1Rect) -> FaceAndOneEye updatedRect eye1Rect
    (Face, FaceAndTwoEyes _ eye1Rect eye2Rect) -> FaceAndTwoEyes updatedRect eye1Rect eye2Rect
    (Eye1, FaceAndOneEye faceRect _) -> FaceAndOneEye faceRect updatedRect
    (Eye1, FaceAndTwoEyes faceRect _ eye2Rect) -> FaceAndTwoEyes faceRect updatedRect eye2Rect
    (Eye2, FaceAndTwoEyes faceRect eye1Rect _) -> FaceAndTwoEyes faceRect eye1Rect updatedRect
    _ -> shapes


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewLocation location ->
      case String.split "|" location.hash of
        "" :: [] ->
          ( model
          , Http.send NewFileItems
            (Http.get ("/files/") fileItemsDecoder)
          )

        "#files" :: path :: [] ->
          ( model
          , Http.send NewFileItems
            (Http.get ("/files" ++ path) fileItemsDecoder)
          )

        "#annotate" :: path :: [] ->
          let
            imagePath : List String
            imagePath =
              List.filterMap Http.decodeUri (List.filter (not << String.isEmpty) (String.split "/" path))
          in
            ( { model
              | path = imagePath
              , workingAnnotation =
                Just
                { imageSize = Dimension 0 0
                , scaleDown = always 0
                , scaleUp = always 0
                , shapes = Nothing
                , modified = False
                , mouseDragState = Nothing
                }
              , message = Nothing
              }
            , Cmd.batch
              [ Http.send NewMetadata
                ( Http.get ( "/metadata" ++ pathSpec imagePath ) metadataDecoder )
              , Http.send NewAnnotation
                ( Http.get ( "/annotation" ++ pathSpec imagePath ) annotationsDecoder )
              ]
            )

        _ ->
          ( model
          , Navigation.modifyUrl "/"
          )

    NewFileItems (Ok files) ->
      ( { model
        | path = files.path
        , fileItems = List.sortBy .name files.fileItems
        , workingAnnotation = Nothing
        , message = Nothing
        }
      , Cmd.none
      )

    NewFileItems (Err err) ->
      ( { model | message = Just (Err (toString err)) }
      , Cmd.none
      )

    NewAnnotation (Ok {annotations}) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | shapes =
              case (List.filter ((==) "face" << .label) annotations, List.filter ((==) "eye" << .label) annotations) of
                ([face], []) ->
                  Just (FaceOnly face.shape)
                ([face], [eye1]) ->
                  Just (FaceAndOneEye face.shape (relativize face.shape eye1.shape))
                ([face], [eye1, eye2]) ->
                  Just (FaceAndTwoEyes face.shape (relativize face.shape eye1.shape) (relativize face.shape eye2.shape))
                _ ->
                  Nothing
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    NewAnnotation (Err err) ->
      ( case err of
          (Http.BadStatus ({status})) as err ->
            if status.code == 404 then model -- No annotation just means the image hasn't been annotated
            else { model | message = Just (Err (toString err)) }

          _ -> { model | message = Just (Err (toString err)) }
      , Cmd.none
      )

    NewMetadata (Ok {size}) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | imageSize = size
            , scaleDown  = toFloat >> (*) ( toFloat scaledWidthPx / toFloat size.widthPixel ) >> round
            , scaleUp  = toFloat >> (*) ( toFloat size.widthPixel / toFloat scaledWidthPx ) >> round
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    NewMetadata (Err err) ->
      ( { model | message = Just (Err (toString err)) }
      , Cmd.none
      )

    DragToCreateBoxStart {clientPos, offsetPos} ->
      case Maybe.andThen .shapes model.workingAnnotation of
        Just (FaceAndTwoEyes _ _ _) ->
          ( { model | message = Just (Err "IMPOSSIBLE STATE: DragToCreateBoxStart when all shapes present") }
          , Cmd.none
          )

        curShapes ->
          ( { model
            | workingAnnotation =
              Maybe.map
              ( \annotation ->
                let
                  (xPixel, yPixel) = offsetPos

                  loc : Point
                  loc =
                    Point
                    (annotation.scaleUp (round xPixel))
                    (annotation.scaleUp (round yPixel))

                  rect : Rectangle
                  rect = Rectangle loc (Dimension 0 0)

                  (newShapes, shapeId, constrainTo) =
                    case curShapes of
                      Nothing ->
                        (Just (FaceOnly rect), Face, annotation.imageSize)
                      Just (FaceOnly faceRect) ->
                        (Just (FaceAndOneEye faceRect rect), Eye1, faceRect.size)
                      Just (FaceAndOneEye faceRect eye1Rect) ->
                        (Just (FaceAndTwoEyes faceRect eye1Rect rect), Eye2, faceRect.size)
                      Just (FaceAndTwoEyes _ _ _) ->
                        -- The Elm compiler does not do this, but this cannot happen
                        (Nothing, Face, annotation.imageSize)

                  minSize : Dimension
                  minSize = Dimension 0 0

                  maxLen : Int
                  maxLen = min (constrainTo.widthPixel - loc.xPixel) (constrainTo.heightPixel - loc.yPixel)

                  maxSize : Dimension
                  maxSize = Dimension maxLen maxLen
                in
                  { annotation
                  | shapes = newShapes
                  , mouseDragState = Just (Resizing (updateShapeInShapes shapeId) rect minSize maxSize clientPos)
                  }
              )
              model.workingAnnotation
            }
          , Cmd.none
          )

    DragToResizeBoxStart shapeId mousePos ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | mouseDragState =
              case Maybe.andThen (\shapes -> Maybe.map (\rect -> (shapes, rect)) (getShape shapeId shapes)) annotation.shapes of
                Just (shapes, rect) ->
                  let
                    minSize : Dimension
                    minSize = Dimension 0 0 -- TODO face needs to contain eyes

                    constrainTo : Dimension
                    constrainTo =
                      case (shapeId, shapes) of
                        (Eye1, FaceAndOneEye faceRect _) -> faceRect.size
                        (Eye1, FaceAndTwoEyes faceRect _ _) -> faceRect.size
                        (Eye2, FaceAndTwoEyes faceRect _ _) -> faceRect.size
                        _ -> annotation.imageSize

                    maxLen : Int
                    maxLen =
                      min
                      (constrainTo.widthPixel - rect.location.xPixel)
                      (constrainTo.heightPixel - rect.location.yPixel)

                    maxSize : Dimension
                    maxSize = Dimension maxLen maxLen
                  in
                    Just (Resizing (updateShapeInShapes shapeId) rect minSize maxSize mousePos)

                Nothing -> Nothing
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    DragToResizeBoxMove (xPixel, yPixel) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            case (annotation.shapes, annotation.mouseDragState) of
              (Just shapes, Just (Resizing updateShapes baseRect minSize maxSize (baseXPixel, baseYPixel))) ->
                let
                  baseSize : Dimension
                  baseSize = baseRect.size

                  candidateWidthPixel : Int
                  candidateWidthPixel = annotation.scaleUp (round (xPixel - baseXPixel)) + baseSize.widthPixel

                  candidateHeightPixel : Int
                  candidateHeightPixel = annotation.scaleUp (round (yPixel - baseYPixel)) + baseSize.heightPixel

                  preConstrainedSizePx : Int
                  preConstrainedSizePx = max candidateWidthPixel candidateHeightPixel

                  minSizePx : Int
                  minSizePx = max minSize.widthPixel minSize.heightPixel

                  maxSizePx : Int
                  maxSizePx = min maxSize.widthPixel maxSize.heightPixel

                  sizePx : Int
                  sizePx = max minSizePx (min maxSizePx preConstrainedSizePx)
                in
                  { annotation
                  | shapes = Just (updateShapes { baseRect | size = Dimension sizePx sizePx } shapes)
                  , modified = True
                  }

              _ -> annotation
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    DragToMoveBoxStart shapeId mousePos ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | mouseDragState =
              case Maybe.andThen (getShape shapeId) annotation.shapes of
                Just rect -> Just (Moving (updateShapeInShapes shapeId) rect mousePos)
                Nothing -> Nothing
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    DragToMoveBoxMove (xPixel, yPixel) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            case (annotation.shapes, annotation.mouseDragState) of
              (Just shapes, Just (Moving updateShapes baseRect (baseXPixel, baseYPixel))) ->
                let
                  baseLocation : Point
                  baseLocation = baseRect.location

                  xPixelDelta : Int
                  xPixelDelta = annotation.scaleUp (round (xPixel - baseXPixel))

                  yPixelDelta : Int
                  yPixelDelta = annotation.scaleUp (round (yPixel - baseYPixel))
                in
                  { annotation
                  | shapes =
                    Just
                    ( updateShapes
                      { baseRect
                      | location = Point (baseLocation.xPixel + xPixelDelta) (baseLocation.yPixel + yPixelDelta)
                      }
                      shapes
                    )
                  , modified = True
                  }

              _ -> annotation
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    DragStop ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation -> { annotation | mouseDragState = Nothing } )
          model.workingAnnotation
        }
      , Cmd.none
      )

    SubmitAnnotationRequest ->
      ( { model | message = Nothing }
      , case Maybe.andThen .shapes model.workingAnnotation of
          Just shapes ->
            let
              labeledShapes : List (String, Rectangle)
              labeledShapes =
                case shapes of
                  FaceOnly faceRect ->
                    [ ("face", faceRect) ]
                  FaceAndOneEye faceRect eye1Rect ->
                    [ ("face", faceRect), ("eye", absolutize faceRect eye1Rect) ]
                  FaceAndTwoEyes faceRect eye1Rect eye2Rect ->
                    [ ("face", faceRect), ("eye", absolutize faceRect eye1Rect), ("eye", absolutize faceRect eye2Rect) ]
            in
              Http.send SubmitAnnotationResponse
              ( Http.post
                ( "/annotation" ++ pathSpec model.path ++ "?"
                ++model.csrfToken.tokenName ++ "=" ++ model.csrfToken.tokenValue
                )
                ( Http.jsonBody
                  ( Encode.object
                    [ ( "annotations"
                      , Encode.list
                        ( List.map
                          ( \(label, rect) ->
                            Encode.object
                            [ ( "label", Encode.string label )
                            , ( "shape"
                              , Encode.object
                                [ ( "leftPixel", Encode.int rect.location.xPixel )
                                , ( "topPixel", Encode.int rect.location.yPixel )
                                , ( "widthPixel", Encode.int rect.size.widthPixel )
                                , ( "heightPixel", Encode.int rect.size.heightPixel )
                                ]
                              )
                            ]
                          )
                          labeledShapes
                        )
                      )
                    ]
                  )
                )
                annotationsDecoder
              )

          Nothing -> Cmd.none
      )

    SubmitAnnotationResponse (Ok metadata) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation -> { annotation | modified = False } )
          model.workingAnnotation
        , message = Just ( Ok "Annotation saved" )
        }
      , Cmd.none
      )

    SubmitAnnotationResponse (Err err) ->
      ( { model | message = Just (Err (toString err)) }
      , Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


shapesView : (Int -> Int) -> Maybe MouseDragState -> Shapes -> List (Html Msg)
shapesView scaleDown mouseDragState shapes =
  let
    faceRect : Rectangle
    faceRect =
      case shapes of
        FaceOnly faceRect -> faceRect
        FaceAndOneEye faceRect _ -> faceRect
        FaceAndTwoEyes faceRect _ _ -> faceRect

    eyeRects : List (ShapeId, Rectangle)
    eyeRects =
      case shapes of
        FaceOnly faceRect -> []
        FaceAndOneEye _ eye1Rect -> [ (Eye1, eye1Rect) ]
        FaceAndTwoEyes _ eye1Rect eye2Rect -> [ (Eye1, eye1Rect), (Eye2, eye2Rect) ]

    topLeft : Point
    topLeft = faceRect.location

    dimension : Dimension
    dimension = faceRect.size
  in
    [ div
      ( [ class "shape"
        , style
          [ ( "left", toString (scaleDown topLeft.xPixel) ++ "px" )
          , ( "top", toString (scaleDown topLeft.yPixel) ++ "px" )
          , ( "width", toString ((scaleDown dimension.widthPixel) - 4) ++ "px" )
          , ( "height", toString ((scaleDown dimension.heightPixel) - 4) ++ "px" )
          , let
              cursor : String
              cursor =
                case (mouseDragState, shapes) of
                  (Nothing, FaceAndTwoEyes _ _ _) -> "default"
                  (Nothing, _) -> "crosshair"
                  (Just (Resizing _ _ _ _ _), _) -> "nwse-resize"
                  (Just (Moving _ _ _ ), _) -> "grabbing"
            in ( "cursor", cursor )
          ]
        ]
      ++case (mouseDragState, shapes) of
          (Nothing, FaceAndTwoEyes _ _ _) ->
            [ onDown (always DragStop) ]
          (Nothing, _) -> -- FaceOnly/FaceAndOneEye
            [ onDown DragToCreateBoxStart ]
          (Just (Resizing _ _ _ _ _), _) ->
            [ onMove ( .clientPos >> DragToResizeBoxMove )
            , onUp ( always DragStop )
            ]
          (Just (Moving _ _ _), _) ->
            [ onMove ( .clientPos >> DragToMoveBoxMove )
            , onUp ( always DragStop )
            ]
      )
      ( [ div
          [ class "move"
          , onDown ( .clientPos >> (DragToMoveBoxStart Face) )
          ]
          [ text "🐶" ]
        , div
          [ class "resize"
          , onDown ( .clientPos >> (DragToResizeBoxStart Face) )
          ]
          []
        ]
      ++( List.map
          ( \(shapeId, eyeRect) ->
            let
              topLeft : Point
              topLeft = eyeRect.location

              dimension : Dimension
              dimension = eyeRect.size
            in
              div
              [ class "shape"
              , style
                [ ( "left", toString ((scaleDown topLeft.xPixel) - 2) ++ "px" )
                , ( "top", toString ((scaleDown topLeft.yPixel) - 2) ++ "px" )
                , ( "width", toString ((scaleDown dimension.widthPixel) - 4) ++ "px" )
                , ( "height", toString ((scaleDown dimension.heightPixel) - 4) ++ "px" )
                , let
                    cursor : String
                    cursor =
                      case mouseDragState of
                        Nothing -> "default"
                        Just (Resizing _ _ _ _ _) -> "nwse-resize"
                        Just (Moving _ _ _) -> "grabbing"
                  in ( "cursor", cursor )
                ]
              ]
              [ div
                [ class "move"
                , onDown ( .clientPos >> (DragToMoveBoxStart shapeId) )
                ]
                [ text "👁" ]
              , div
                [ class "resize"
                , onDown ( .clientPos >> (DragToResizeBoxStart shapeId) )
                ]
                []
              ]
          )
          eyeRects
        )
      )
    ]


view : Model -> Html Msg
view model =
  div []
  [ p [ id "pathNavigation" ]
    ( let
        pathReversed : List String
        pathReversed = List.reverse model.path

        pathElems : List { name : String, path : List String }
        pathElems =
          List.scanl
          ( \name -> \pathElem ->
            { name = name, path = pathElem.path ++ [ name ] }
          )
          { name = "🏠", path = [] }
          ( List.reverse ( List.drop 1 pathReversed ) )
      in
        ( List.concatMap
          ( \pathElem ->
            [ a [ href ( "#files|" ++ pathSpec pathElem.path ++ "/" ) ] [ text pathElem.name ]
            , text " / "
            ]
          )
          pathElems
        ++case List.head pathReversed of
          Just lastPathElemName -> [ text lastPathElemName ]
          Nothing -> []
        )
    )
  , hr [] []
  , div [ id "message" ]
    ( List.filterMap
      ( Maybe.map
        ( \message ->
          case message of
            Ok infoMessage ->
              div [ class "info" ] [ text infoMessage ]
            Err errorMessage ->
              div [ class "error" ] [ text errorMessage ]
        )
      )
      [ model.message ]
    )
  , div []
    ( case model.workingAnnotation of
      Just { imageSize, scaleDown, shapes, modified, mouseDragState } ->
        [ div
          ( [ id "annotation"
            , style [ ( "position", "relative" ) ]
            ]
          ++case mouseDragState of
              Nothing ->
                case shapes of
                  Nothing ->
                    [ onDown DragToCreateBoxStart
                    , style [ ( "cursor", "crosshair" ) ]
                    ]
                  _ ->
                    [ onDown (always DragStop) ]
              Just (Resizing _ _ _ _ _) ->
                [ onMove ( .clientPos >> DragToResizeBoxMove )
                , onUp ( always DragStop )
                ]
              Just (Moving _ _ _) ->
                [ onMove ( .clientPos >> DragToMoveBoxMove )
                , onUp ( always DragStop )
                ]
          )
          ( img
            [ src ( "/image" ++ pathSpec model.path ++ ".jpg" )
            , width scaledWidthPx
            ]
            []
          ::case shapes of
            Just shapes -> shapesView scaleDown mouseDragState shapes
            Nothing -> []
          )
        , div []
          [ button [ onClick SubmitAnnotationRequest, disabled (not modified) ] [ text "Save" ]
          ]
        , div []
          [ text ("Image size: " ++ (toString imageSize.widthPixel) ++ "x" ++ (toString imageSize.heightPixel)) ]
        ]

      Nothing ->
        let
          (dirs, files) = List.partition .dir model.fileItems
        in
          [ ul []
            ( List.map
              ( \dir ->
                li []
                [ a
                  [ href ( "#files|" ++ pathSpec model.path ++ "/" ++ dir.name ++ "/" ) ]
                  [ text dir.name ]
                ]
              )
              dirs
            )
          , hr [] []
          , div [ id (String.join "|" model.path) ]
            ( List.map
              (\file ->
                div [ class "thumbnail" ]
                [ a [ href ( "#annotate|" ++ pathSpec model.path ++ "/" ++ file.name ) ]
                  [ img
                    [ id file.name
                    , src ( "/image" ++ pathSpec model.path ++ "/" ++ file.name ++ ".jpg" )
                    , title file.name
                    , width 325
                    ]
                    []
                  ]
                ]
              )
              files
            )
          ]
    )
  ]


main : Program CsrfToken Model Msg
main =
  Navigation.programWithFlags
  NewLocation
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
