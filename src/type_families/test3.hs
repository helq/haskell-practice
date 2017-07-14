-- example taken from https://stackoverflow.com/questions/45029144/when-do-i-need-type-annotations
-- for more info read answers

{-# LANGUAGE TypeFamilies #-}

import Foundation
import Data.Text
import GHC.Num

{-# ANN module ("HLint: ignore Use print"::Text) #-}

--default () -- removes defaulting of 25 to Integer

tryMe :: (Num a) => Maybe a -> a -> a
tryMe = flip fromMaybe

class Test a where
    type TT a
    doIt :: TT a -> a -> a

instance Test Int where
    type TT Int = Maybe Int
    doIt = flip fromMaybe

main :: IO ()
--main = putStrLn . show $ tryMe (Just 2) 25
main = putStrLn . show $ tryMe (Just 2) (25::Int)
--main = putStrLn . show $ doIt (Just 2) (25::Int)
--main = putStrLn . show $ (doIt (Just 2) 25::Int)
--main = putStrLn . show $ doIt (Just 2) 25 -- fails

-- the code next, also fails, but why?
-- answer, type families (or better, type functions) aren't injective
--main = putStrLn . show $ doIt (Just (2::Int)) 25
