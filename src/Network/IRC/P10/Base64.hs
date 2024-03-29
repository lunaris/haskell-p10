module Network.IRC.P10.Base64 where

import Data.Functor
import Data.List
import Data.Maybe

type Base64Char
  = Char

type Base64String
  = String

base64Chars :: [Base64Char]
base64Chars
  = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "[]"

base64CharToInt :: Base64Char -> Maybe Int
base64CharToInt c
  = lookup c (zip base64Chars [0..])

class IsBase64 a where
  fromBase64 :: Base64String -> Maybe a

instance IsBase64 Int where
  fromBase64 s
    = foldl' ((+) .  (64 *)) 0 <$> mapM base64CharToInt s
