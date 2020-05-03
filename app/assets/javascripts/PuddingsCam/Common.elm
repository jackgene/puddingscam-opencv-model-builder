module PuddingsCam.Common exposing (..)

import Http
import Mouse
import Navigation


-- Constants
targetScaledWidthPx : Int
targetScaledWidthPx = 1024


defaultScaleFactor : Float
defaultScaleFactor = 0.16


scaleFactors : List Float
scaleFactors = defaultScaleFactor :: [0.25, 0.33, 0.5, 0.66, 1.0]


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
type alias Image =
  { originalSize : Dimension
  , scaledSize : Dimension
  , scaleFactor : Float
  }
type alias WorkingAnnotation =
  { image : Maybe Image
  , persistedShapes : Maybe Shapes
  , workingShapes : Maybe Shapes
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
  | NewMetadata (Result Http.Error Metadata)
  | NewAnnotation (Result Http.Error Annotations)
  | NewAnnotationSuggestion (Result Http.Error Annotations)
  -- Change magnification
  | ChangeScaleFactorTo Float
  -- Edit annotation mouse events
  | DragToCreateBoxStart Mouse.Event
  | DragToResizeBoxStart ShapeId MousePos
  | DragToResizeBoxMove MousePos
  | DragToMoveBoxStart ShapeId MousePos
  | DragToMoveBoxMove MousePos
  | DragStop
  -- Clear annotation
  | ClearAnnotations
  -- Edit annotation save/cancel events
  | SubmitAnnotationRequest
  | SubmitAnnotationResponse (Result Http.Error Annotations)
  -- Misc
  | NoOp


-- Common functions
pathSpec : List String -> String
pathSpec path =
  String.concat (List.map ((++) "/" << Http.encodeUri) path)


scale : Float -> Int -> Int
scale scaleFactor = toFloat >> (*) scaleFactor >> round
