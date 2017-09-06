-- This has nothing to do with the rest of the repo.
-- eval "$(echo -n ' cat'; printf ' | ./dvorakize%.0s' {1..84})"

module Main where

import Data.List (elemIndex)
import Data.Char (toLower)
import System.IO (hSetBuffering, BufferMode(LineBuffering), stdin, stdout)

prencode = "abcdefghijklmnopqrstuvwxyz.,'; "
encoding = "axje.uidchtnmbrl'poygk,qf;vwzs "

convChar c = maybe c ((!!) encoding) $ elemIndex (toLower c) prencode

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  interact $ map conv

test = "the quick brown fox jumped over the lazy dog,.';"

countCycles = length $ takeWhile ((/=) test) $ drop 1 (iterate (map conv) test)
