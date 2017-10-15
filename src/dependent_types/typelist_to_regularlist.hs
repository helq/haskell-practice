{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Data.Proxy (Proxy(Proxy))
import Data.Monoid ((<>))
import GHC.TypeLits

main :: IO ()
main = do
  fromNat     (undefined :: Proxy 5)
  fromListNat (undefined :: Proxy '[2,3,10])

fromNat :: KnownNat n => Proxy n -> IO ()
fromNat proxy = do
  let (num :: Integer) = natVal proxy -- converting a Nat to an Integer
  putStrLn $ "Some num: " <> show num

-- CODE FROM ANSWER: https://stackoverflow.com/a/46751209/1744344
-- Similar to `KnownNat (n :: Nat)`
class KnownNatList (ns :: [Nat]) where
   natListVal :: proxy ns -> [Integer]

-- Base case
instance KnownNatList '[] where
  natListVal _ = []

-- Inductive step
instance (KnownNat n, KnownNatList ns) => KnownNatList (n ': ns) where
  natListVal _ = natVal (Proxy :: Proxy n) : natListVal (Proxy :: Proxy ns)

fromListNat :: KnownNatList ns => Proxy ns -> IO ()
fromListNat proxy = do
  let (listNum :: [Integer]) = natListVal proxy
  putStrLn $ "Some list of num: " <> show listNum
