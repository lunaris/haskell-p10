{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.IRC.P10.Syntax where

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

data Message
  = PassM   { _mPass            :: String
            }

  | ServerM { _mServerName      :: String
            , _mBootTimestamp   :: String
            , _mLinkTimestamp   :: String
            , _mProtocol        :: String
            , _mServerNumeric   :: Numeric ServerT
            , _mMaxConnections  :: Int
            , _mFlags           :: Maybe String
            , _mDescription     :: String
            }

  deriving (Eq, Show)
