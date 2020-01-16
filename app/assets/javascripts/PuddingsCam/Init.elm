module PuddingsCam.Init exposing (..)

import PuddingsCam.Common exposing (CsrfToken, Model, Msg(NewLocation))
import Navigation
import Task


init : CsrfToken -> Navigation.Location -> (Model, Cmd Msg)
init csrfToken location =
  ( Model csrfToken [] [] Nothing Nothing
  , Task.perform NewLocation (Task.succeed location)
  )
