module PuddingsCam.View exposing (view)

import PuddingsCam.Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, height, href, id, selected, src, style, title, value, width)
import Html.Events exposing (onClick, onInput)
import Mouse exposing (onDown, onMove, onUp)


shapesView : Float -> Maybe MouseDragState -> Shapes -> Bool -> List (Html Msg)
shapesView scaleFactor mouseDragState shapes unsaved =
  let
    pixelSize : Int -> Int
    pixelSize size = max 1 ((scale scaleFactor size) - 4)

    faceRect : Rectangle
    faceRect =
      case shapes of
        FaceOnly faceRect -> faceRect
        FaceAndOneEye faceRect _ -> faceRect
        FaceAndTwoEyes faceRect _ _ -> faceRect

    eyeRects : List (ShapeId, Rectangle)
    eyeRects =
      case shapes of
        FaceOnly _ -> []
        FaceAndOneEye _ eye1Rect -> [ (Eye1, eye1Rect) ]
        FaceAndTwoEyes _ eye1Rect eye2Rect -> [ (Eye1, eye1Rect), (Eye2, eye2Rect) ]

    faceTopLeft : Point
    faceTopLeft = faceRect.location

    faceDimension : Dimension
    faceDimension = faceRect.size
  in
    [ div
      ( [ class ("shape" ++ if unsaved then " unsaved" else "")
        , style
          [ ( "left", toString (scale scaleFactor faceTopLeft.xPixel) ++ "px" )
          , ( "top", toString (scale scaleFactor faceTopLeft.yPixel) ++ "px" )
          , ( "width", toString (pixelSize faceDimension.widthPixel) ++ "px" )
          , ( "height", toString (pixelSize faceDimension.heightPixel) ++ "px" )
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
              eyeTopLeft : Point
              eyeTopLeft = eyeRect.location

              eyeDimension : Dimension
              eyeDimension = eyeRect.size
            in
              div
              [ class ("shape cross" ++ if unsaved then " unsaved" else "")
              , style
                [ ( "left", toString ((scale scaleFactor eyeTopLeft.xPixel) - 2) ++ "px" )
                , ( "top", toString ((scale scaleFactor eyeTopLeft.yPixel) - 2) ++ "px" )
                , ( "width", toString (pixelSize eyeDimension.widthPixel) ++ "px" )
                , ( "height", toString (pixelSize eyeDimension.heightPixel) ++ "px" )
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
        Just { image, shapes, unsaved, mouseDragState } ->
          div [ class "content-scrollable" ]
          [ div [ id "toolbar" ]
            [ button [ onClick SubmitAnnotationRequest, disabled (not unsaved) ] [ text "💾" ]
            , case image of
              Just { scaleFactor } ->
                select
                [ onInput
                  ( \scaleFactorStr ->
                    case String.toFloat scaleFactorStr of
                      Ok scaleFactor -> ChangeScaleFactorTo scaleFactor
                      _ -> NoOp
                  )
                ]
                ( List.map
                  ( \scaleFactorOpt ->
                    option
                    [ value (toString scaleFactorOpt), selected (scaleFactorOpt == scaleFactor) ]
                    [ text (toString (scaleFactorOpt * 100) ++ "%") ]
                  )
                  scaleFactors
                )
              Nothing ->
                select [ disabled True ] []
            ]
          , div [ id "workspace" ]
            ( case image of
              Just { originalSize, scaledSize, scaleFactor } ->
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
                    , title (((toString originalSize.widthPixel) ++ "x" ++ (toString originalSize.heightPixel)))
                    , width scaledSize.widthPixel
                    , height scaledSize.heightPixel
                    ]
                    []
                  ::case shapes of
                    Just shapes -> shapesView scaleFactor mouseDragState shapes unsaved
                    Nothing -> []
                  )
                ]

              Nothing ->
                [ text "Loading..." ]
            )
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
              ( ( List.map
                  (\file ->
                    div [ class "thumbnail" ]
                    ( a [ href ( "#annotate|" ++ pathSpec model.path ++ "/" ++ file.name ) ]
                      [ img
                        [ id file.name
                        , src ( "/image" ++ pathSpec model.path ++ "/" ++ file.name ++ ".jpg" )
                        , title file.name
                        , width 325
                        ]
                        []
                      ]
                    ::case file.numAnnotations of
                      Just numAnnotations ->
                        [ div [ class "label" ] [ text (toString numAnnotations) ] ]
                      Nothing -> []
                    )
                  )
                  files
                )
              ++[ div [ class "spacer" ] [] ]
              )
            ]
      ]
    )
  ]
