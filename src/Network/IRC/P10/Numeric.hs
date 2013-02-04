{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.IRC.P10.Numeric where

import Network.IRC.P10.Base64

import Control.Applicative
import Data.String

data NumericType
  = ServerT
  | ClientT
  deriving (Eq, Show)

data Numeric :: NumericType -> * where
  ServerN :: Int -> Numeric ServerT
  ClientN :: Numeric ServerT -> Int -> Numeric ClientT

numericToInt :: Numeric a -> Int
numericToInt (ServerN sn)
  = sn

numericToInt (ClientN _ cn)
  = cn

serverNumeric :: Numeric ClientT -> Numeric ServerT
serverNumeric (ClientN sn _)
  = sn

clientNumericToInts :: Numeric ClientT -> (Int, Int)
clientNumericToInts (ClientN (ServerN sn) cn)
  = (sn, cn)

deriving instance Eq (Numeric a)
deriving instance Show (Numeric a)

instance IsBase64 (Numeric ServerT) where
  fromBase64 sn@[_, _]
    = ServerN <$> fromBase64 sn

  fromBase64 _
    = Nothing

instance IsBase64 (Numeric ClientT) where
  fromBase64 (s1 : s2 : cn@[_, _, _])
    = ClientN <$> fromBase64 [s1, s2] <*> fromBase64 cn

  fromBase64 _
    = Nothing
