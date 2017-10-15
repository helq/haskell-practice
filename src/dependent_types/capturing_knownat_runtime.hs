{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude hiding (replicate)
import Data.Proxy (Proxy(Proxy))
import Data.Monoid ((<>))
import Data.Singletons (SomeSing(..), toSing)
import GHC.TypeLits
import Data.Singletons.TypeLits
import Data.Vector.Sized (Vector, replicate)

main :: IO ()
main = playingWithTypes 8

playingWithTypes :: Integer -> IO ()
playingWithTypes nn = do

  case someNatVal nn of
    Just (SomeNat (proxy :: Proxy n)) -> do
      let (num :: Integer) = natVal proxy
      putStrLn $ "Some num: " <> show num
      putStrLn $ "Some vector: " <> show (replicate 5 :: Vector n Int)
    Nothing -> putStrLn "There's no number, the integer was not a natural number"

  case (toSing nn :: SomeSing Nat) of
    SomeSing (SNat :: Sing n) -> do
      let (num :: Integer) = natVal (Proxy :: Proxy n)
      putStrLn $ "Some num: " <> show num
      putStrLn $ "Some vector: " <> show (replicate 5 :: Vector n Int)
