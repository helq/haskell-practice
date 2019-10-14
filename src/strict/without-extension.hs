-- Example taken from: https://www.reddit.com/r/haskell/comments/3sts2t/strict_haskell_xstrict_has_landed/cx0f4fq/
-- take a look at with-strict-extension.hs to see what happens when -XStrict is used

{-# LANGUAGE BangPatterns #-}

module Main where

import Debug.Trace (trace)
import Data.Monoid ((<>))

data D = D !Int Int

f :: Int -> D
f  0 = D 0 undefined
f !i = D i (i `mod` 2)

stupidShow :: D -> String
stupidShow (D !i !_) = "D " <> show i

topLevel :: Int
topLevel = trace "evaluating toplevel" 10

main :: IO ()
main = do
    let !x = trace "building x" (f 0)
        !y = x `seq` trace "building y" (f 10)

    putStrLn "Should have already failed!!"

    putStrLn (stupidShow y)
    putStrLn (stupidShow x)
