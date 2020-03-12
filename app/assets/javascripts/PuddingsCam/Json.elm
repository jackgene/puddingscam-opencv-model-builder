module PuddingsCam.Json exposing (..)

import PuddingsCam.Common exposing (..)
import Json.Decode as Decode


-- JSON
fileItemDecoder : Decode.Decoder FileItem
fileItemDecoder =
  Decode.map3 FileItem
  (Decode.field "name" Decode.string)
  (Decode.field "dir" Decode.bool)
  (Decode.maybe (Decode.field "numAnnotations" Decode.int))


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
