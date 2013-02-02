module Network.IRC.P10.Base64 where

import Data.Maybe

type Base64Char
  = Char

type Base64String
  = String

base64Chars :: [Base64Char]
base64Chars
  = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "[]"

base64CharToInt :: Base64Char -> Int
base64CharToInt c
  = fromJust (lookup c (zip base64Chars [0..]))
