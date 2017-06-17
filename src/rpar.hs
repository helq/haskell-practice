{-
 - How to run:
 -    ghc rpar.hs -threaded
 -    ./rpar 1 +RTS -N2
 - or with stack:
 -    stack build
 -    stack exec -- rpar 1 +RTS -N2
 -}

import Foundation
import Foundation.String.Read      (readIntegral)
import Control.Parallel.Strategies (runEval, rseq, rpar)
import Control.Exception           (evaluate)
import Data.Time.Clock             (diffUTCTime, getCurrentTime, UTCTime)
import Text.Printf                 (printf)

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

-- <<main
main = do
  n <- takeNumber <$> getArgs
  let (test:_) = drop (n-1) [test1,test2,test3,test4]
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printf "test%d\n" n
  printTimeSince t0
  print r
  printTimeSince t0

takeNumber :: [String] -> Int
takeNumber (x:_) = maybe 1 between1and4 $ readIntegral x
  where between1and4 i | i>=1 && i<=4 = i
                       | otherwise    = 1
takeNumber []    = 1

print :: Show a => a -> IO ()
print = putStrLn . show
-- >>

-- <<test1
test1 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x,y)
-- >>

-- <<test2
test2 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x,y)
-- >>

-- <<test3
test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  rseq x
  return (x,y)
-- >>

-- <<test4
test4 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  rseq x
  rseq y
  return (x,y)
-- >>

printTimeSince :: UTCTime -> IO ()
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
