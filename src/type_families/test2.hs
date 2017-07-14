{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import Foundation
import Data.Text

class AClass t where
  type AType t :: *
  somefunction :: t -> AType t

instance AClass Text where
  type AType Text = Int
  somefunction = const 4

{-
 -instance AClass String where
 -  type AType String = Int
 -  somefunction = const 5
 -}

instance AClass Int where
  type AType Int = Integer
  somefunction = const 3

main :: IO ()
main = putStrLn . (show::Int->String) $ (somefunction :: Text -> AType Text) ""

{- For more info on why isn't GHC able to infer the types correctly look at
 - https://stackoverflow.com/questions/45069784/cannot-match-type-of-expression-using-typefamilies
-}
