{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MyModule (
  MyClass (..),
  MyType (MyCon, conField1, conField2),
  func1,
  (*+*),
) where

import Control.Monad (when)

-- | This is a comment
class MyClass a where
  foo :: a -> a

data MyType a = MyCon {conField1 :: Int, conField2 :: a}

func1 :: forall a. (MonadIO m, Show a) => a -> [a] -> m (a, [a])
func1 a [] = pure (a, [])
func1 a (_ : end) = do
  debug <- getEnv "DEBUG"
  when debug $ print end
  pure (a, end)

(*+*) :: Int -> Int -> Bool
a *+* b = (a * 2) == b && odd a
