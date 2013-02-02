module Network.IRC.P10.Syntax where

data Message
  = PassMessage   { _mPass            :: String
                  }

  | ServerMessage { _mServerName      :: String
                  , _mBootTimestamp   :: String
                  , _mLinkTimestamp   :: String
                  , _mProtocol        :: String
                  , _mServerNumeric   :: String
                  , _mMaxConnections  :: String
                  , _mFlags           :: Maybe String
                  , _mDescription     :: String
                  }

  deriving (Eq, Show)

splitNumeric :: String -> (String, String)
splitNumeric [c1, c2, c3, c4, c5]
  = ([c1, c2], [c3, c4, c5])
