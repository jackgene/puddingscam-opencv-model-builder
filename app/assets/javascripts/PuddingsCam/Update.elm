module PuddingsCam.Update exposing (update)

import PuddingsCam.Common exposing (..)
import PuddingsCam.Json exposing (..)
import Http
import Json.Encode as Encode
import Navigation


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
                { image = Nothing
                , persistedShapes = Nothing
                , workingShapes = Nothing
                , unsaved = False
                , mouseDragState = Nothing
                }
              , message = Just (Ok "Awaiting metadata and annotations...")
              }
            , Cmd.batch
              [ Http.send NewMetadata
                ( Http.get ( "/metadata" ++ pathSpec imagePath ) metadataDecoder )
              , Http.send NewAnnotation
                ( Http.get ( "/annotations" ++ pathSpec imagePath ) annotationsDecoder )
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

    NewMetadata (Ok {size}) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | image =
              Just
              ( let
                  targetScaleFactor : Float
                  targetScaleFactor = (toFloat targetScaledWidthPx) / (toFloat size.widthPixel)

                  scaleFactor : Float
                  scaleFactor =
                    Maybe.withDefault defaultScaleFactor
                    (List.maximum (List.filter ((>=) targetScaleFactor) scaleFactors))
                in
                  { originalSize = size
                  , scaledSize =
                    { widthPixel = scale scaleFactor size.widthPixel
                    , heightPixel = scale scaleFactor size.heightPixel
                    }
                  , scaleFactor = scaleFactor
                  }
              )
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

    NewAnnotation (Ok {annotations}) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            let
              shapes : Maybe Shapes
              shapes =
                case (List.filter ((==) "face" << .label) annotations, List.filter ((==) "eye" << .label) annotations) of
                  ([face], []) ->
                    Just (FaceOnly face.shape)
                  ([face], [eye1]) ->
                    Just (FaceAndOneEye face.shape (relativize face.shape eye1.shape))
                  ([face], [eye1, eye2]) ->
                    Just (FaceAndTwoEyes face.shape (relativize face.shape eye1.shape) (relativize face.shape eye2.shape))
                  _ ->
                    Nothing
            in { annotation | persistedShapes = shapes, workingShapes = shapes }
          )
          model.workingAnnotation
        , message = Nothing
        }
      , Cmd.none
      )

    NewAnnotation (Err err) ->
      case err of
        (Http.BadStatus ({status})) as err ->
          if status.code == 404 then -- No annotation means the image hasn't been annotated, get suggestions
            ( { model | message = Just (Ok "Awaiting suggested annotations...") }
            , Http.send NewAnnotationSuggestion
              ( Http.get ( "/annotations" ++ pathSpec model.path ++ "?suggested=true" ) annotationsDecoder )
            )
          else ( { model | message = Just (Err (toString err)) }, Cmd.none )

        _ -> ( { model | message = Just (Err (toString err)) }, Cmd.none )

    NewAnnotationSuggestion (Ok {annotations}) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | workingShapes =
              case (List.filter ((==) "face" << .label) annotations, List.filter ((==) "eye" << .label) annotations) of
                ([face], []) ->
                  Just (FaceOnly face.shape)
                ([face], [eye1]) ->
                  Just (FaceAndOneEye face.shape (relativize face.shape eye1.shape))
                ([face], [eye1, eye2]) ->
                  Just (FaceAndTwoEyes face.shape (relativize face.shape eye1.shape) (relativize face.shape eye2.shape))
                _ ->
                  Nothing
            , unsaved = True
            }
          )
          model.workingAnnotation
        , message = Nothing
        }
      , Cmd.none
      )

    NewAnnotationSuggestion (Err err) ->
      ( case err of
        (Http.BadStatus ({status})) as err ->
          if status.code == 404 then { model | message = Nothing } -- No suggestions
          else { model | message = Just (Err (toString err)) }

        _ -> { model | message = Just (Err (toString err)) }
      , Cmd.none
      )

    ChangeScaleFactorTo scaleFactor ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | image =
              Maybe.map
              ( \image ->
                { image
                | scaledSize =
                  { widthPixel = scale scaleFactor image.originalSize.widthPixel
                  , heightPixel = scale scaleFactor image.originalSize.heightPixel
                  }
                , scaleFactor = scaleFactor
                }
              )
              annotation.image
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    DragToCreateBoxStart {clientPos, offsetPos} ->
      case (Maybe.andThen .workingShapes model.workingAnnotation, Maybe.andThen .image model.workingAnnotation) of
        (_, Nothing) ->
          ( { model | message = Just (Err "IMPOSSIBLE STATE: DragToCreateBoxStart when metadata not loaded") }
          , Cmd.none
          )

        (Just (FaceAndTwoEyes _ _ _), _) ->
          ( { model | message = Just (Err "IMPOSSIBLE STATE: DragToCreateBoxStart when all shapes present") }
          , Cmd.none
          )

        (curShapes, Just image) ->
          ( { model
            | workingAnnotation =
              Maybe.map
              ( \annotation ->
                let
                  (xPixel, yPixel) = offsetPos

                  loc : Point
                  loc =
                    Point
                    (scale (1 / image.scaleFactor) (round xPixel))
                    (scale (1 / image.scaleFactor) (round yPixel))

                  rect : Rectangle
                  rect = Rectangle loc (Dimension 0 0)

                  (newShapes, shapeId, constrainTo) =
                    case curShapes of
                      Nothing ->
                        (Just (FaceOnly rect), Face, image.originalSize)
                      Just (FaceOnly faceRect) ->
                        (Just (FaceAndOneEye faceRect rect), Eye1, faceRect.size)
                      Just (FaceAndOneEye faceRect eye1Rect) ->
                        (Just (FaceAndTwoEyes faceRect eye1Rect rect), Eye2, faceRect.size)
                      Just (FaceAndTwoEyes _ _ _) ->
                        -- The Elm compiler does not do this, but this cannot happen
                        (Nothing, Face, Dimension 0 0)

                  minSize : Dimension
                  minSize = Dimension 0 0

                  maxLen : Int
                  maxLen = min (constrainTo.widthPixel - loc.xPixel) (constrainTo.heightPixel - loc.yPixel)

                  maxSize : Dimension
                  maxSize = Dimension maxLen maxLen
                in
                  { annotation
                  | workingShapes = newShapes
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
              let
                shapesAndShape : Maybe (Shapes, Rectangle)
                shapesAndShape =
                  Maybe.andThen
                  ( \shapes ->
                    Maybe.map (\rect -> (shapes, rect)) (getShape shapeId shapes)
                  )
                  annotation.workingShapes
              in case (shapesAndShape, annotation.image) of
                (Just (shapes, rect), Just image) ->
                  let
                    minSize : Dimension
                    minSize =
                      case (shapeId, shapes) of
                        (Face, FaceAndOneEye _ eye1Rect) ->
                          Dimension
                          (eye1Rect.location.xPixel + eye1Rect.size.widthPixel)
                          (eye1Rect.location.yPixel + eye1Rect.size.heightPixel)

                        (Face, FaceAndTwoEyes _ eye1Rect eye2Rect) ->
                          Dimension
                          ( max
                            (eye1Rect.location.xPixel + eye1Rect.size.widthPixel)
                            (eye2Rect.location.xPixel + eye2Rect.size.widthPixel)
                          )
                          ( max
                            (eye1Rect.location.yPixel + eye1Rect.size.heightPixel)
                            (eye2Rect.location.yPixel + eye2Rect.size.heightPixel)
                          )

                        _ ->  Dimension 0 0

                    constrainTo : Dimension
                    constrainTo =
                      case (shapeId, shapes) of
                        (Eye1, FaceAndOneEye faceRect _) -> faceRect.size
                        (Eye1, FaceAndTwoEyes faceRect _ _) -> faceRect.size
                        (Eye2, FaceAndTwoEyes faceRect _ _) -> faceRect.size
                        _ -> image.originalSize

                    maxLen : Int
                    maxLen =
                      min
                      (constrainTo.widthPixel - rect.location.xPixel)
                      (constrainTo.heightPixel - rect.location.yPixel)

                    maxSize : Dimension
                    maxSize = Dimension maxLen maxLen
                  in
                    Just (Resizing (updateShapeInShapes shapeId) rect minSize maxSize mousePos)

                _ -> Nothing
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    DragToResizeBoxMove (curXPixel, curYPixel) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            case (annotation.workingShapes, annotation.image, annotation.mouseDragState) of
              (Just shapes, Just image, Just (Resizing updateShapes baseRect minSize maxSize (baseXPixel, baseYPixel))) ->
                let
                  baseSize : Dimension
                  baseSize = baseRect.size

                  candidateWidthPixel : Int
                  candidateWidthPixel = scale (1 / image.scaleFactor) (round (curXPixel - baseXPixel)) + baseSize.widthPixel

                  candidateHeightPixel : Int
                  candidateHeightPixel = scale (1 / image.scaleFactor) (round (curYPixel - baseYPixel)) + baseSize.heightPixel

                  preConstrainedSizePx : Int
                  preConstrainedSizePx = max candidateWidthPixel candidateHeightPixel

                  minSizePx : Int
                  minSizePx = max minSize.widthPixel minSize.heightPixel

                  maxSizePx : Int
                  maxSizePx = min maxSize.widthPixel maxSize.heightPixel

                  sizePx : Int
                  sizePx = max minSizePx (min maxSizePx preConstrainedSizePx)

                  updatedShapes : Maybe Shapes
                  updatedShapes = Just (updateShapes { baseRect | size = Dimension sizePx sizePx } shapes)
                in
                  { annotation
                  | workingShapes = updatedShapes
                  , unsaved = annotation.persistedShapes /= updatedShapes
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
              let
                shapeAndBounds : Maybe (Rectangle, Dimension)
                shapeAndBounds =
                  Maybe.andThen
                  ( \shapes ->
                    Maybe.andThen
                    ( \rect ->
                      case shapeId of
                        Face ->
                          Maybe.map
                          (\image -> (rect, image.originalSize))
                          annotation.image
                        _ ->
                          Maybe.map
                          (\faceRect -> (rect, faceRect.size))
                          (getShape Face shapes)
                    )
                    (getShape shapeId shapes)
                  )
                  annotation.workingShapes
              in case shapeAndBounds of
                Just (rect, bounds) ->
                  let
                    bottomRightBound : Point
                    bottomRightBound =
                      Point (bounds.widthPixel - rect.size.widthPixel) (bounds.heightPixel - rect.size.widthPixel)
                  in
                    Just (Moving (updateShapeInShapes shapeId) rect bottomRightBound mousePos)

                Nothing -> Nothing
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    DragToMoveBoxMove (curXPixel, curYPixel) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            case (annotation.workingShapes, annotation.image, annotation.mouseDragState) of
              (Just shapes, Just image, Just (Moving updateShapes baseRect bottomRightBound (baseXPixel, baseYPixel))) ->
                let
                  baseLocation : Point
                  baseLocation = baseRect.location

                  xPixelDelta : Int
                  xPixelDelta = scale (1 / image.scaleFactor) (round (curXPixel - baseXPixel))

                  yPixelDelta : Int
                  yPixelDelta = scale (1 / image.scaleFactor) (round (curYPixel - baseYPixel))

                  preConstrainedXPixel : Int
                  preConstrainedXPixel = baseLocation.xPixel + xPixelDelta

                  preConstrainedYPixel : Int
                  preConstrainedYPixel = baseLocation.yPixel + yPixelDelta

                  xPixel : Int
                  xPixel = max 0 (min bottomRightBound.xPixel preConstrainedXPixel)

                  yPixel : Int
                  yPixel = max 0 (min bottomRightBound.yPixel preConstrainedYPixel)
                in
                  { annotation
                  | workingShapes =
                    Just
                    ( updateShapes
                      { baseRect | location = Point xPixel yPixel }
                      shapes
                    )
                  , unsaved = True
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

    ClearAnnotations ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | workingShapes = Nothing
            , unsaved = annotation.persistedShapes /= Nothing
            }
          )
          model.workingAnnotation
        }
      , Cmd.none
      )

    SubmitAnnotationRequest ->
      ( { model | message = Nothing }
      , case Maybe.andThen .workingShapes model.workingAnnotation of
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
                ( "/annotations" ++ pathSpec model.path ++ "?"
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

          Nothing ->
            Http.send (always (SubmitAnnotationResponse (Ok (Annotations []))))
            ( Http.request
              { method = "DELETE"
              , headers = []
              , url =
                ( "/annotations" ++ pathSpec model.path ++ "?"
                ++model.csrfToken.tokenName ++ "=" ++ model.csrfToken.tokenValue
                )
              , body = Http.emptyBody
              , expect = Http.expectString
              , timeout = Nothing
              , withCredentials = False
              }
            )
      )

    SubmitAnnotationResponse (Ok _) ->
      ( { model
        | workingAnnotation =
          Maybe.map
          ( \annotation ->
            { annotation
            | persistedShapes = annotation.workingShapes
            , unsaved = False
            }
          )
          model.workingAnnotation
        , message = Just ( Ok "Annotation saved" )
        }
      , Cmd.none
      )

    SubmitAnnotationResponse (Err err) ->
      ( { model | message = Just (Err (toString err)) }
      , Cmd.none
      )

    NoOp ->
      ( { model | message = Just (Err "IMPOSSIBLE STATE: NoOp should never be used") }
      , Cmd.none
      )