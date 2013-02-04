{-# LANGUAGE DataKinds #-}

module Network.IRC.P10.Syntax where

import Network.IRC.P10.Numeric

data Message
  = PassM
      { _mPass            :: String
      }

  | ServerM
      { _mServerName      :: String
      , _mBootTimestamp   :: String
      , _mLinkTimestamp   :: String
      , _mProtocol        :: String
      , _mServerNumeric   :: Numeric ServerT
      , _mMaxConnections  :: Int
      , _mFlags           :: Maybe String
      , _mDescription     :: String
      }

  | EndBurstM
      { _mServerNumeric   :: Numeric ServerT
      }

  | EndBurstAckM
      { _mServerNumeric   :: Numeric ServerT
      }

  deriving (Eq, Show)
