module UI.Controllers.Screens.Dashboard where

import Prelude 
import Remote.Types (Currency(..), GetPriceForCryptosReq(..), GetPriceForCryptosRes(..), ResCoin(..))

import Data.Array (filter, head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (Self, StateUpdate(..), runUpdate)
import UI.Utils (callApiAndUpdateState, isEqual)

send :: Self Props State -> Action -> Effect Unit
send = runUpdate update

type Props = Unit

type Coin =
  { name :: String
  , quantity :: Number
  , price :: { usd :: Number, inr :: Number }
  }

type State =
  { coins :: Array Coin
  }

initialState :: State
initialState =
  { coins
  }

coins :: Array Coin
coins = 
  [ { name:"BTC"
    , quantity: 0.0526
    , price: { usd: 0.0, inr: 0.0 }
    }
  , { name:"DIW"
    , quantity: 52000.0
    , price: { usd: 0.0, inr: 0.0 }
    }
  , { name:"LTC"
    , quantity: 0.3793
    , price: { usd: 0.0, inr: 0.0 }
    }
  , { name:"NGC"
    , quantity: 108.0
    , price: { usd: 0.0, inr: 0.0 }
    }
  ]

didMount :: Self Props State -> Effect Unit
didMount self = do
  let req = GetPriceForCryptosReq 
        { cryptoNames: (\coin -> coin.name) <$> coins
        , currencies: [ "USD", "INR" ]
        }

  callApiAndUpdateState send self LoadState req 
    (\st (GetPriceForCryptosRes res) -> st { coins = (updatePrices res.results) <$> self.state.coins })

  where
    updatePrices results coin = case head (filter (\(ResCoin c) -> isEqual c.name coin.name ) results) of
        Nothing -> coin
        Just (ResCoin c) -> do
          let curUsd = head (filter (\(Currency cur) -> isEqual cur.name "USD") c.prices)
          let curInr = head (filter (\(Currency cur) -> isEqual cur.name "INR") c.prices)
          case curUsd, curInr of
            Just (Currency usd), Just (Currency inr) -> coin { price = { usd: usd.value, inr: inr.value } }
            _, _ -> coin


data Action
  = Noop
  | LoadState State
  | Coin Int

update :: Self Props State -> Action -> StateUpdate Props State
update self@{ state } = case _ of
  Coin i -> NoUpdate
  LoadState st -> Update st
  _ -> NoUpdate
