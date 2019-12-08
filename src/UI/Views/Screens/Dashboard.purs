module UI.Views.Screens.Dashboard where

import Prelude
import UI.Controllers.Screens.Dashboard

import Data.Array (foldl, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Global (toFixed)
import React.Basic (JSX, Self, createComponent, make, runUpdate)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as Events
import React.Basic.Events (EventHandler)
import UI.Utils (classy, icony)
import UI.Views.Components.Header as Header

screen :: Props -> JSX
screen = make (createComponent "Dashboard") 
  { initialState
  , didMount
  , render
  }

send :: Self Props State -> Action -> Effect Unit
send = runUpdate update

render :: Self Props State -> JSX
render self@{ state } = Header.component
    { title: "My Cryptos"
    , links: []
    , user: Nothing
    , content: [ content self ]
    }

content :: Self Props State -> JSX
content self@{ state } = classy DOM.div "mdl-grid"
  [ classy DOM.div "mdl-cell mdl-cell--1-col-tablet mdl-cell--3-col-desktop mdl-cell--0-col-phone" []
  , classy DOM.div "mdl-cell mdl-cell--6-col-tablet mdl-cell--6-col-desktop mdl-cell--4-col-phone"
    [ classy DOM.div "mdl-card mdl-shadow--4dp"
      [ classy DOM.div "w-100 mdl-card__supporting-text"
        [ classy DOM.table "w-100 mdl-data-table mdl-js-data-table"
          [ DOM.thead_
            [ DOM.tr_
              [ DOM.th_ [ DOM.text "S.No." ]
              , classy DOM.th "mdl-data-table__cell--non-numeric" [ DOM.text "Coin" ]
              , DOM.th_ [ DOM.text "Price" ]
              , DOM.th_ [ DOM.text "Quantity" ]
              , DOM.th_ [ DOM.text "Amount" ]
              ]
            ]
          , DOM.tbody_ (mapWithIndex coinRow state.coins)
          ]
        , DOM.hr {}
        , DOM.h4_
          [ DOM.text "Total"
          , classy DOM.span "float-right"
            [ icony "fa fa-inr" "" "1rem"
            , classy DOM.span "text-dark font-weight-bold" [ DOM.text (" " <> (fromMaybe "" $ toFixed 2 totalAmount)) ] 
            ]
          ]
        , DOM.hr {}
        ]
      ]
    ]
  , classy DOM.div "mdl-cell mdl-cell--1-col-tablet mdl-cell--3-col-desktop mdl-cell--0-col-phone" []
  ]
  where
    action :: Action -> EventHandler
    action = Events.capture_ <<< send self

    totalAmount :: Number
    totalAmount = foldl (\b coin -> coinAmount coin + b) 0.0 state.coins

    coinAmount :: Coin -> Number
    coinAmount coin = coin.price.inr * coin.quantity

    coinRow :: Int -> Coin -> JSX
    coinRow i coin = do
      let usd = fromMaybe "" $ toFixed 2 $ coin.price.usd
      let inr = fromMaybe "" $ toFixed 2 $ coin.price.inr * coin.quantity
      DOM.tr
        { onClick: action (Coin i)
        , children: 
          [ DOM.td_ [ DOM.text (show $ i + 1) ]
          , classy DOM.td "mdl-data-table__cell--non-numeric" 
            [ classy DOM.span "font-weight-bold" [ DOM.text coin.name ] ]
          , DOM.td_ [ DOM.text ("$ " <> usd) ]
          , DOM.td_ [ DOM.text (show coin.quantity) ]
          , DOM.td_ [ DOM.text ("₹ " <> inr) ]
          ]
        }
      