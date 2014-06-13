{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module DomEff where

import           Control.Monad

type Type = String
type Text = String
type Id   = String
type Prop = String

class DomEff e m | m -> e where
  newElem     :: Type -> m e
  newTextElem :: Text -> m e
  elemById    :: Id   -> m (Maybe e)

  setProp     :: e -> Id -> String -> m ()
  getProp     :: e -> Id -> m String
  setAttr     :: e -> Id -> String -> m ()
  getAttr     :: e -> Id -> m String

  -- @addChild c p@ adds @c@ as the last child of @p@
  addChild       :: e -> e -> m ()
  -- @addChild c p t@ adds @c@ to @p@ before @t@ which must be
  addChildBefore :: e -> e -> e -> m ()

  -- @removeChild c p@ removes the child @c@ from @p@
  removeChild    :: e -> e -> m ()
  clearChildren  :: e -> m ()

  getChildBefore :: e -> m (Maybe e)
  getLastChild   :: e -> m (Maybe e)
  getChildren    :: e -> m [e]
  setChildren    :: e -> [e] -> m ()

  -- getFile
  -- getFileName

  getStyle       :: e -> Id -> m String
  setStyle       :: e -> Id -> String -> m ()

  setClass       :: e -> String -> Bool -> m ()
  toggleClass    :: e -> String -> m ()
  hasClass       :: e -> String -> m Bool

withElem :: (Monad m, DomEff e m) => Id -> (e -> m a) -> m (Maybe a)
withElem i f = do
  e <- elemById i
  case e of
    Nothing -> return Nothing
    Just a  -> liftM Just (f a)

withElems :: (Monad m, DomEff e m) => [Id] -> ([e] -> m a) -> m (Maybe a)
withElems is f = do
  es <- mapM elemById is
  case sequence es of
    Nothing -> return Nothing
    Just as  -> liftM Just (f as)
