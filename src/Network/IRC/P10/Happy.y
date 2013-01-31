{
module Network.IRC.P10.Happy where

import Network.IRC.P10.Alex
import Network.IRC.P10.Syntax
}

%name                     message PostAuthMessage
%tokentype                { Token }
%error                    { parseError }

%monad                    { LP }
%lexer                    { lexToken } { EOFTk }

%token
  " "                     { SpaceTk }
  ":"                     { ColonTk }
  newline                 { NewlineTk }
  command                 { StringTk $$ }
  base64                  { StringTk $$ }
  lastToken               { StringTk $$ }

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

PostAuthMessage
  : base64 " " command " " ":" lastToken
                          { PostAuthMessage $1 $3 [$6] }
  ;

{
parseError :: Token -> LP a
parseError _
  = fail "Parse error!"

parseMessage :: String -> Either String PostAuthMessage
parseMessage
  = evalLP message
}
