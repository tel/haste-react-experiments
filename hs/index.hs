{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{- // createClass
 - 1. Bind all prototype functions to new object
 - 2. Copy props
 - 3. Initialize state if possible
 - 4. Use props.state as base state and overwrite initialized state
 - 5. Store children
 - 6. Set type as "custom"
 - 7. Call initializer
 -}

{- // renderComponent
 - 1. Call component.render to create a virtualDOM
 - 2. State changed=false
 -
 - When there is not current a mountId for the given component:
 - 1. Create unique mountId
 - 2. Create a mount div
 - 3. Label mount with:
 -    - mountId
 -    - virtual dom element created above
 - 4. Embed rendered virtual dom inside div
 - 5. Put mount into target node
 - 6. Call componentDidRender
 -
 - When there is already a mountId, we've rendered this before
 - 1. Find DOM element for previous mount
 - 2. Extract previous mount's virtualDOM
 - 3. Compare virtualDOM equality (have we changed?)
 -    If so:
 -    1. render new virtual dom
 -    2. changed = true
 -    3. clear mount
 -    4. append new rendered child
 -    5. replace old cached virtual dom
 -    6. call componentDidRender
 -
 - Then we return whether or not we changed the element
 -}

{- // Virtual Dom Equality
 - Includes:
 - - props
 - - children
 -
 - NOT state. State gets rerendered upon update alone.
 -}

{- // renderVirtualDOM
 - If there's not a type then it's a text node and render that, else
 -
 - 1. Create Element of type type
 - 2. Copy props and children
 - 3. For each prop:
 -    If begins with "on" then add an EventListener to the element
 -    If it is "style" then write all of the styles to the element
 -    Else, store the prop on the element
 - 4. For each child
 -    render the child and append to the node
 -
 -}

import           Control.Lens
import           Control.Monad
import           Data.Hashable
import           Data.Map      (Map)
import qualified Data.Map      as Seq
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Typeable
import           GHC.Generics

--------------------------------------------------------------------------------
-- Virtual DOM

type ElType    = String
type Attribute = String

data Vdom
  = Text String
  | Node String (Map String String) (Seq Vdom)
  deriving ( Typeable )
