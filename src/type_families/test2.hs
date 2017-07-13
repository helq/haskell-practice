{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text

class AClass t where
  type AType t :: *
  somefunction :: t -> AType t

instance AClass Text where
  type AType Text = Int
  somefunction = const 4

instance AClass Int where
  type AType Int = Integer
  somefunction = const 3

main :: IO ()
main = putStrLn . (show::Int->String) $ (somefunction :: Text -> AType Text) ""
