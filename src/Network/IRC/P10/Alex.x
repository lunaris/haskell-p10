{
module Network.IRC.P10.Alex where

import Network.IRC.P10.Position

import Control.Monad
import Data.Word
}

$digit                      = [0-9]
$letter                     = [a-zA-Z]
$symbol                     = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]

@character                  = $digit | $letter | $symbol
@space                      = " "
@newline                    = \n | \r\n
@colonString                = ":" (@character | @space)*
@string                     = @character+

tokens :-
  $white+                   ;
  @colonString              { colonString }
  @string                   { string }
{
type AlexInput
  = (Position, Word8, String)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (_, _, [])
  = Nothing

alexGetByte (pos, _, (c : cs))
  = Just (w, (movePosition pos c, w, cs))
    where
      w = fromIntegral (ord c)

alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar (_, w, _)
  = w

type StartCode
  = Int

data LPState
  = LPState { lpPosition  :: !Position
            , lpPrevious  :: !Word8
            , lpInput     :: String
            , lpStartCode :: !StartCode
            }
  deriving (Eq, Show)

newtype LP a
  = LP { runLP :: LPState -> Either String (a, LPState) }

evalLP :: LP a -> String -> Either String a
evalLP (LP f) input
  = case f initial of
      Left err ->
        Left err

      Right (x, _) ->
        Right x

    where
      initial
        = LPState { lpPosition  = initialPosition
                  , lpPrevious  = 0
                  , lpInput     = input
                  , lpStartCode = 0
                  }

instance Functor LP where
  {-# INLINE fmap #-}
  fmap
    = liftM

instance Monad LP where
  {-# INLINE (>>=) #-}
  m >>= f
    = LP $ \s ->
        case runLP m s of
          Left err ->
            Left err

          Right (x, s') ->
            runLP (f x) s'

  {-# INLINE return #-}
  return x
    = LP $ \s -> Right (x, s)

  {-# INLINE fail #-}
  fail err
    = LP $ \_ -> Left err

getAlexInput :: LP AlexInput
getAlexInput
  = LP $ \s@LPState { lpPosition = pos, lpPrevious = w, lpInput = input } ->
      Right ((pos, w, input), s)

setAlexInput :: AlexInput -> LP ()
setAlexInput (pos, w, input)
  = LP $ \s ->
      Right ((), s { lpPosition = pos, lpPrevious = w, lpInput = input })

getPosition :: LP Position
getPosition
  = LP $ \s@LPState { lpPosition = pos } -> Right (pos, s)

getStartCode :: LP StartCode
getStartCode
  = LP $ \s@LPState { lpStartCode = code } -> Right (code, s)

setStartCode :: StartCode -> LP ()
setStartCode code
  = LP $ \s ->
      Right ((), s { lpStartCode = code })

scanToken :: LP Token
scanToken = do
  alexInput@(_, _, input) <- getAlexInput
  code <- getStartCode

  case alexScan alexInput code of
    AlexEOF ->
      return EOFTk

    AlexError _ ->
      LP $ \_ -> Left "Lexical error"

    AlexSkip alexInput' _ -> do
      setAlexInput alexInput'
      scanToken

    AlexToken alexInput' len action -> do
      setAlexInput alexInput'
      action (take len input)

lexToken :: (Token -> LP a) -> LP a
lexToken k
  = scanToken >>= k

type Action a
  = String -> LP a

type Variable
  = Char

type Constructor
  = String

data Token
  = ColonStringTk String
  | StringTk String
  | EOFTk
  deriving (Eq, Show)

{-# INLINE colonString #-}
colonString :: Action Token
colonString
  = return . ColonStringTk . tail

{-# INLINE string #-}
string :: Action Token
string
  = return . StringTk
}
