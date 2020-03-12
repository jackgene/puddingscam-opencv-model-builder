module PuddingsCam.Common exposing (..)

import Http
import Mouse
import Navigation


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
  , numAnnotations : Maybe Int
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
  = Moving (Rectangle -> Shapes -> Shapes) Rectangle Point MousePos
  | Resizing (Rectangle -> Shapes -> Shapes) Rectangle Dimension Dimension MousePos
type alias WorkingAnnotation =
  { imageSize : Dimension
  , scaleDown : Int -> Int
  , scaleUp : Int -> Int
  , shapes : Maybe Shapes
  , unsaved : Bool
  , mouseDragState : Maybe MouseDragState
  }
type alias Model =
  { csrfToken: CsrfToken
  , path : List String
  , fileItems : List FileItem
  , workingAnnotation : Maybe WorkingAnnotation
  , message : Maybe (Result String String)
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
  | NewAnnotationSuggestion (Result Http.Error Annotations)
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


-- Common functions
pathSpec : List String -> String
pathSpec path =
  String.concat (List.map ((++) "/" << Http.encodeUri) path)
