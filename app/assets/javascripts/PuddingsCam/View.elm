module PuddingsCam.View exposing (view)

import PuddingsCam.Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, id, src, style, title, width)
import Html.Events exposing (onClick)
import Mouse exposing (onDown, onMove, onUp)


shapesView : (Int -> Int) -> Maybe MouseDragState -> Shapes -> List (Html Msg)
shapesView scaleDown mouseDragState shapes =
  let
    pixelSize : Int -> Int
    pixelSize size = max 1 ((scaleDown size) - 4)

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
          , ( "width", toString (pixelSize dimension.widthPixel) ++ "px" )
          , ( "height", toString (pixelSize dimension.heightPixel) ++ "px" )
          , let
              cursor : String
              cursor =
                case (mouseDragState, shapes) of
                  (Nothing, FaceAndTwoEyes _ _ _) -> "default"
                  (Nothing, _) -> "crosshair"
                  (Just (Resizing _ _ _ _ _), _) -> "nwse-resize"
                  (Just (Moving _ _ _ _), _) -> "grabbing"
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
          (Just (Moving _ _ _ _), _) ->
            [ onMove ( .clientPos >> DragToMoveBoxMove )
            , onUp ( always DragStop )
            ]
      )
      ( [ div
          [ class "move"
          , style
            [ let
                cursor : String
                cursor =
                  case mouseDragState of
                    Just (Moving _ _ _ _) -> "grabbing"
                    _ -> "grab"
              in ( "cursor", cursor )
            ]
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
                , ( "width", toString (pixelSize dimension.widthPixel) ++ "px" )
                , ( "height", toString (pixelSize dimension.heightPixel) ++ "px" )
                , let
                    cursor : String
                    cursor =
                      case mouseDragState of
                        Nothing -> "default"
                        Just (Resizing _ _ _ _ _) -> "nwse-resize"
                        Just (Moving _ _ _ _) -> "grabbing"
                  in ( "cursor", cursor )
                ]
              ]
              [ div
                [ class "move"
                , style
                  [ let
                      cursor : String
                      cursor =
                        case mouseDragState of
                          Just (Moving _ _ _ _) -> "grabbing"
                          _ -> "grab"
                    in ( "cursor", cursor )
                  ]
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
  div [ id "app" ]
  [ div [ id "pathNavigation" ]
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
        ++( case List.head pathReversed of
            Just lastPathElemName -> [ text lastPathElemName ]
            Nothing -> []
          )
        ++[ hr [] [] ]
        )
    )
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
  , div [ class "content-container" ]
    ( List.map (always (div [ class "content-placeholder" ] [])) model.path
    ++[ case model.workingAnnotation of
        Just { imageSize, scaleDown, shapes, modified, mouseDragState } ->
          div [ class "content-scrollable" ]
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
                Just (Moving _ _ _ _) ->
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
            div [ class "content-scrollable" ]
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
            , div []
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
      ]
    )
  ]
