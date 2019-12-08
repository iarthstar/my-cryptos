module UI.Views.Components.Header where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import React.Basic (JSX, Self, createComponent, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events as Events
import UI.Controllers.Components.Header (Props, State, initialState)
import UI.Utils (anchory, classy)

component :: Props -> JSX
component = make (createComponent "header")
  { render
  , initialState 
  }

render :: Self Props State -> JSX
render self@{ props } = classy DOM.div "mdl-layout mdl-js-layout mdl-layout--fixed-header"
  [ classy DOM.header "mdl-layout__header"
    [ classy DOM.div "mdl-layout__header-row"
      [ classy DOM.span "mdl-layout-title"
        [ DOM.text props.title ]
      , classy DOM.div "mdl-layout-spacer" []
      , classy DOM.nav "mdl-navigation mdl-layout--large-screen-only" $ makeLinks props.links
      , loggedUserView
      ]
    ]
  , classy DOM.div "mdl-layout__drawer"
    [ classy DOM.span "mdl-layout-title"
      [ DOM.text props.title ]
    , classy DOM.nav "mdl-navigation" $ makeLinks props.links
    ]
  , classy DOM.main "mdl-layout__content"
    [ classy DOM.div "page-content" props.content
    ]
  ]
  where
    loggedUserView = case props.user of
      Nothing   -> classy DOM.span "" []
      Just user -> DOM.span
          { id: "user-menu"
          , style: DOM.css { cursor: "pointer" }
          , children: 
            [ classy DOM.button "mdl-button mdl-js-button mdl-button--icon mr-2"
              [ DOM.img 
                { src: "https://images.unsplash.com/photo-1454789548928-9efd52dc4031?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1600&q=80" 
                , width: "32px"
                , height: "32px"
                } 
              ] 
            , DOM.text user
            , DOM.ul 
              { className: "mdl-menu mdl-menu--bottom-right mdl-js-menu mdl-js-ripple-effect"
              , htmlFor: "user-menu"
              , children: [ classy DOM.li "mdl-menu__item" [ DOM.text "Log Out" ] ]
              }
            ]
          }
                    

makeLinks :: Array (Tuple String String) -> Array JSX
makeLinks = map (\(Tuple name href) -> anchory "mdl-navigation__link" name href)