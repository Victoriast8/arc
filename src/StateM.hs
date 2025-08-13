module StateM 
  ( StateM,
  )
where

import Control.Monad (ap,liftM)

type State a = [(String, a)]

newtype StateM a = (State a -> (Either Error (State a)))

instance Functor StateM where
  fmap = liftM

instance Applicative StateM where
  pure x = StateM $ \state -> (Right state)
  (<*>) = ap

instance Monad StateM where
  EvalM x >>= f = StateM $ \state ->
    case x state of
      Left e -> Left e
      Right x -> Right x
