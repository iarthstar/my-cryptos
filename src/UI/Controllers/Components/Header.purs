module UI.Controllers.Components.Header where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import React.Basic (JSX, Self, StateUpdate(..), runUpdate)

send :: Self Props State -> Action -> Effect Unit
send = runUpdate update

type Props = 
  { title :: String
  , links :: Array (Tuple String String)
  , user :: Maybe String
  , content :: Array JSX
  }

type State = Unit

initialState :: State
initialState = unit

data Action
  = Noop
  | LoadState State

update :: Self Props State -> Action -> StateUpdate Props State
update self@{ state } action = case action of
  _ -> NoUpdate