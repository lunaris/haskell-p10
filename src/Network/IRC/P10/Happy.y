{
module Network.IRC.P10.Happy where

import Network.IRC.P10.Alex
import Network.IRC.P10.Syntax
}

%name                     message PassMessage
%tokentype                { Token }
%error                    { parseError }

%monad                    { LP }
%lexer                    { lexToken } { EOFTk }

%token
  "0"                     { StringTk "0" }
  "1"                     { StringTk "1" }

  PASS                    { StringTk "PASS" }
  SERVER                  { StringTk "SERVER" }
  N                       { StringTk "N" }

  base64Numeric           { StringTk $$ }
  lastToken               { ColonStringTk $$ }

  serverName              { StringTk $$ }
  bootTimestamp           { StringTk $$ }
  linkTimestamp           { StringTk $$ }
  protocol                { StringTk $$ }
  flags                   { StringTk $$ }

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

PassMessage
  : PASS lastToken        { PassMessage $2 }
  ;

ServerMessage
  : SERVER serverName "1" bootTimestamp linkTimestamp protocol
      base64Numeric flags lastToken
                          { let (sn, maxConns) = splitNumeric $7
                            in  ServerMessage $2 $4 $5 $6 sn maxConns (Just $8) $9 }
  ;

{
parseError :: Token -> LP a
parseError _
  = fail "Parse error!"

parseMessage :: String -> Either String Message
parseMessage
  = evalLP message
}
