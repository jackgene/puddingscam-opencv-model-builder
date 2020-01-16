import PuddingsCam.Common exposing (CsrfToken, Model, Msg(NewLocation))
import PuddingsCam.Init exposing (init)
import PuddingsCam.Update exposing (update)
import PuddingsCam.View exposing (view)
import Navigation


main : Program CsrfToken Model Msg
main =
  Navigation.programWithFlags
  NewLocation
  { init = init
  , update = update
  , view = view
  , subscriptions = always Sub.none
  }
