{
{-# LANGUAGE GADTs #-}

module Network.IRC.P10.Happy where

import Network.IRC.P10.Alex
import Network.IRC.P10.Base64
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
  N                       { StringTk "N" }

  protocol                { StringTk ('J' : $$) }
  flags                   { StringTk ('+' : $$) }

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

ClientNumeric
  : token                 {%  case $1 of
                                [s1, s2, c1, c2, c3] ->
                                  case (base64StringToInt [s1, s2],
                                    base64StringToInt [c1, c2, c3])  of

                                    (Just sn, Just cn) ->
                                      return (ClientN (ServerN sn) cn)

                                    _ -> fail $ "Parse error: " ++ $1 ++
                                          " is not a valid client numeric"

                                _ ->
                                  fail $ "Parse error: " ++ $1 ++
                                    " is not a valid client numeric"
                          }
  ;

Message
  : PASS lastToken        { PassM $2 }

  | SERVER token "1" token token protocol ClientNumeric
      opt(flags) lastToken

                          { let ClientN sn maxConns = $7
                            in  ServerM $2 $4 $5 $6 sn maxConns $8 $9
                          }

  ;

{
parseError :: Token -> LP a
parseError tok
  = fail $ "Parse error: " ++ show tok

parseMessage :: String -> Either String Message
parseMessage
  = evalLP message
}
