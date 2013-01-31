module Network.IRC.P10.Syntax where

data PostAuthMessage
  = PostAuthMessage
      { _pamPrefix  :: String
      , _pamCommand :: String
      , _pamTokens  :: [String]
      }

    deriving (Eq, Show)
