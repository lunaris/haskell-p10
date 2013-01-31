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
@newline                    = "\n" | "\r\n"

tokens :-
  @space+                   { token SpaceTk }
  @newline                  { token NewlineTk }
  ":"                       { token ColonTk }
  @character+               { string }
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
  = LP $ \s@LPState { lpStartCode = start_code } -> Right (start_code, s)

setStartCode :: StartCode -> LP ()
setStartCode start_code
  = LP $ \s ->
      Right ((), s { lpStartCode = start_code })

scanToken :: LP Token
scanToken = do
  alex_input@(_, _, input) <- getAlexInput
  start_code <- getStartCode

  case alexScan alex_input start_code of
    AlexEOF ->
      return EOFTk

    AlexError _ ->
      LP $ \_ -> Left "Lexical error"

    AlexSkip alex_input' _ -> do
      setAlexInput alex_input'
      scanToken

    AlexToken alex_input' len action -> do
      setAlexInput alex_input'
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
  = SpaceTk
  | NewlineTk
  | ColonTk
  | StringTk String
  | EOFTk
  deriving (Eq, Show)

{-# INLINE token #-}
token :: Token -> Action Token
token tok _
  = return tok

{-# INLINE string #-}
string :: Action Token
string
  = return . StringTk
}
