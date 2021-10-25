{-
HW 5 - Maybe and List Monads
============================

Fill in the `undefined` oparts of these two problems.

Problem - Sequences
-------------------
See [`Sequence.lhs`](Sequence.html)

Problem - Regular Expressions
-----------------------------
See [`RegExp.lhs`](RegExp.html)
-}

module Main where

import qualified RegExp
import qualified Sequence

main :: IO ()
main = do
  putStrLn "This is Sequence"
  Sequence.runTests
  putStrLn "This is RegExp"
  RegExp.runTests
