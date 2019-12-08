module UI.Utils where

import Prelude
import UI.Types (Snackbar)

import Axios (class Axios, axios)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import React.Basic (Self, JSX)
import React.Basic.DOM as DOM

foreign import showSnackbar :: Snackbar -> Effect Unit

foreign import showLoader :: Effect Unit
foreign import hideLoader :: Effect Unit

foreign import isEqual :: String -> String -> Boolean

defaultSnackbar :: Snackbar
defaultSnackbar = 
  { message: "Something went wrong..."
  , timeout: 2750
  , actionHandler: Nothing
  , actionText: Nothing
  }

classy :: ({ className :: String, children :: Array JSX} -> JSX) -> String -> (Array JSX -> JSX)
classy element className children = element { className, children }

anchory :: String -> String -> String -> JSX
anchory className text href = DOM.a { className, href, children: [ DOM.text text ]}

icony :: String -> String -> String -> JSX
icony className name size = DOM.i
    { style: DOM.css { fontSize: size }
    , className: className 
    , children: [ DOM.text name ] 
    }


callApiAndUpdateState :: forall t10 t11 t12 t13 t14
  .  Axios t10 t11 
  => (Self t12 t13 -> t14 -> Effect Unit)
  -> Self t12 t13
  -> (t13 -> t14)
  -> t10
  -> (t13 -> t11 -> t13) 
  -> Effect Unit
callApiAndUpdateState send self action config fn = do 
  showLoader
  launchAff_ do 
    axios config >>= liftEffect <<< case _ of
      Left err  -> showSnackbar defaultSnackbar
      Right res -> send self (action $ fn self.state res)
    liftEffect hideLoader