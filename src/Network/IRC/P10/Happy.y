{
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Network.IRC.P10.Happy where

import Network.IRC.P10.Alex
import Network.IRC.P10.Base64
import Network.IRC.P10.Numeric
import Network.IRC.P10.Syntax
}

%name                     message Message
%tokentype                { Token }
%error                    { parseError }

%monad                    { LP }
%lexer                    { lexToken } { EOFTk }

%token
  "0"                     { StringTk "0" }
  "1"                     { StringTk "1" }

  PASS                    { StringTk "PASS" }
  SERVER                  { StringTk "SERVER" }
  EB                      { StringTk "EB" }

  protocol                { StringTk ('J' : $$) }
  flags                   { StringTk ('+' : $$) }

  serverNumeric           { StringTk $$@[_, _] }
  clientNumeric           { StringTk $$@[_, _, _, _, _] }

  token                   { StringTk $$ }
  lastToken               { ColonStringTk $$ }

%%

opt(x)
  : x                     { Just $1 }
  | {- Empty -}           { Nothing }
  ;

many(x)
  : many1(x)              { $1 }
  | {- Empty -}           { [] }
  ;

many1(x)
  : many1_(x)             { reverse $1 }
  ;

many1_(x)
  : x                     { [$1] }
  | many1_(x) x           { $2 : $1 }
  ;

sep(x, s)
  : sep1(x, s)            { $1 }
  | {- Empty -}           { [] }
  ;

sep1(x, s)
  : sep1_(x, s)           { reverse $1 }
  ;

sep1_(x, s)
  : x                     { [$1] }
  | sep1_(x, s) s x       { $3 : $1 }
  ;

--

ServerNumeric :: { Numeric ServerT }
ServerNumeric
  : serverNumeric
      {%  case fromBase64 $1 of
            Just sn ->  return sn
            _       ->  fail $ "Parse error: " ++ $1 ++
                          " is not a valid server numeric"
      }
  ;

ClientNumeric :: { Numeric ClientT }
ClientNumeric
  : clientNumeric
      {%  case fromBase64 $1 of
            Just cn ->  return cn
            _       ->  fail $ "Parse error: " ++ $1 ++
                          " is not a valid client numeric"
      }
  ;

Message :: { Message }
Message
  : PASS lastToken
      { PassM $2 }

  | SERVER token "1" token token protocol ClientNumeric
      opt(flags) lastToken

      { let ClientN sn maxConns = $7
        in  ServerM $2 $4 $5 $6 sn maxConns $8 $9
      }

  | ServerNumeric EB
      { EndBurstM $1 }
  ;

{
parseError :: Token -> LP a
parseError tok
  = fail $ "Parse error: " ++ show tok

parseMessage :: String -> Either String Message
parseMessage
  = evalLP message
}
