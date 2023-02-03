{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MyModule (
  MyClass (..),
  MyType (MyCon, conField1, conField2),
  func1,
  (*+*),
) where

-- | This is a comment
class MyClass a where
  foo :: a -> a

data MyType a = MyCon {conField1 :: Int, conField2 :: a}

func1 :: forall a. Show a => a -> [a] -> (a, [a])
func1 a [] = (a, [])
func1 a (_ : end) = (a, end)

(*+*) :: Int -> Int -> Bool
a *+* b = (a * 2) == b
