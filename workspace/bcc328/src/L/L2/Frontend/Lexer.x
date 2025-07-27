{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L2.Frontend.Lexer
  ( Alex
  , runAlex
  , alexMonadScan
  , alexError
  , alexGetInput
  , alexSetInput
  , alexSetStartCode
  , lexer
  , Token(..)
  , Lexeme(..)
  , happyLexer
  ) where

import Control.Monad
import Numeric (readDec)
import Data.Char
import Data.Maybe
}

%wrapper "monadUserState"



$digit = 0-9            -- digits 
$alpha    = [a-zA-Z]

-- second RE macros

@number     = $digit+
@identifier = $alpha[$alpha $digit]* -- identifiers

-- tokens declarations

tokens :-
  -- whitespace and comments
    <0> $white+     ;
    <0> "//" .*     ;

    <0> "/*"        {nestComment `andBegin` state_comment}
    <0> "*/"        {\ _ _ -> alexError "Error! Unexpected close comment!"}
    <state_comment> "/*"                     {nestComment}
    <state_comment> "*/"                     {unnestComment}
    <state_comment> .     ;
    <state_comment> \n    ;

  -- String
    <0>             \"           { enterNewString `andBegin` state_string }
    <state_string>  \\n          { addCharToString '\n' }
    <state_string>  \\t          { addCharToString '\t' }


    <state_string>  \\\^[@-_]    { addControlToString }
    <state_string>  \\$digit$digit$digit
                                 { addAsciiToString }
    <state_string>  \\\"         { addCharToString '\"' }
    <state_string>  \\\\         { addCharToString '\\' }
    <state_string>  \\[\ \n\t\f\r\b\v]+\\
                                 ;
    <state_string>  \\           { \_ _ -> alexError  "Illegal escape sequence!" }
    <state_string>  \"           { leaveString `andBegin` state_initial }
    <state_string>  .            { addCurrentToString }
    <state_string>  \n           { skip }

    <state_comment> .                        ;  
    <state_comment> \n                       ;
    <0> @number         {mkNumber}
    <0> "-" @number     {mkNumber}
    <0> ":="            {simpleToken TAssign}
    <0> "("             {simpleToken TLParen}
    <0> ")"             {simpleToken TRParen}
    <0> ";"             {simpleToken TSemi}
    <0> ","             {simpleToken TComma}
    <0> "+"             {simpleToken TPlus}
    <0> "*"             {simpleToken TTimes}
    <0> "-"             {simpleToken TMinus}
    <0> "/"             {simpleToken TDiv}
    <0> "read"          {simpleToken TRead}
    <0> "print"         {simpleToken TPrint}
    <0> "def"           {simpleToken TDef}
    <0> "in"            {simpleToken TIn}
    <0> "end"           {simpleToken TEnd}
    <0> @identifier     {mkIdent}
{



-- user state
data AlexUserState = AlexUserState
  { nestLevel      :: Int
  , stringStartPos :: Maybe AlexPosn
  , stringValue    :: String
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel      = 0
  , stringStartPos = Nothing
  , stringValue    = ""
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put ustate = Alex $ \s -> Right (s{alex_ust=ustate},())

modify :: (AlexUserState->AlexUserState) -> Alex ()
modify f 
  = Alex $ \s->Right(s{alex_ust=f(alex_ust s)},())

getLexerNestLevel :: Alex Int
getLexerNestLevel = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, nestLevel ust)

setLexerNestLevel :: Int -> Alex ()
setLexerNestLevel ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){nestLevel=ss}}, ())

getLexerStringState :: Alex (Maybe AlexPosn)
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringStartPos ust)

setLexerStringState :: Maybe AlexPosn -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){stringStartPos=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){stringValue=ss}}, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){stringValue=c:stringValue (alex_ust s)}}, ())


-- definition of the EOF token

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  userState   <- get
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  when (startCode == state_string) $
    alexError "Error: unclosed string"
  pure $ Token (position pos) TEOF

-- token definition
data Token
  = Token {
      pos :: (Int, Int)
      , lexeme :: Lexeme 
      }deriving (Eq, Ord, Show)

data Lexeme    
    = TNumber Int
    | TString String
    | TAssign
    | TLParen
    | TRParen
    | TPlus
    | TTimes
    | TMinus
    | TDiv
    | TRead
    | TPrint
    | TIdent String
    | TSemi
    | TComma
    | TDef
    | TIn
    | TEnd
    | TEOF

    deriving (Eq, Ord, Show)

-- actions

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkNumber :: AlexAction Token
mkNumber (st, _, _, str) len
  = pure $ Token (position st) (TNumber $ read $ take len str)


mkIdent :: AlexAction Token
mkIdent (st, _, _, str) len 
  = case take len str of
    "read"  -> pure $ Token (position st) TRead
    "print" -> pure $ Token (position st) TPrint
    _ -> pure $ Token (position st) (TIdent (take len str))

state_initial :: Int
state_initial = 0


-- dealing with comments
nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token
unnestComment input len
  = do
      s <- get
      let level = (nestLevel s) - 1
      put s{nestLevel = level}
      when (level == 0) $
        alexSetStartCode 0
      skip input len



-- dealing with strings

enterNewString (p, _, _, _) _ =
    do setLexerStringState (Just p)
       setLexerStringValue ""
       alexMonadScan

addCharToString :: Char -> AlexAction Token
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan

addCurrentToString :: AlexAction Token
addCurrentToString i@(_,_,_,c:_) 1 =
  addCharToString c i 1
addCurrentToString _ _ =
  error "addCurrentToString: expected exactly one input character"


-- if we are given the special form '\nnn'
addAsciiToString :: AlexAction Token
addAsciiToString i@(_,_,_,'\\':d1:d2:d3:[]) 4 =
  let s = [d1,d2,d3]
      [(v,"")] = readDec s
      c        = chr v
  in if v < 256
        then addCharToString c i 4
        else alexError "Error: Invalid ascii value"
addAsciiToString _ _ =
  error "addAsciiToString: expected '\\nnn' escape of length 4"

-- if we are given the special form '\^A'
addControlToString :: AlexAction Token
addControlToString i@(_,_,_,'^':c:_) 2 =
  let v  = ord c
      c' = if v >= 64
             then chr (v - 64)
             else error "addControlToString: control code out of range"
  in addCharToString c' i 2
addControlToString _ _ =
  error "addControlToString: expected '\\^X' escape of length 2"

leaveString :: AlexAction Token
leaveString _ _ = do
  s  <- getLexerStringValue
  mp <- getLexerStringState       -- posição da aspa de abertura
  setLexerStringState Nothing     -- limpa o estado de string
  let startPos = fromJust mp
      str      = reverse s
  -- aqui construímos um Token, não um Lexeme
  pure $ Token (position startPos) (TString str)

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) _
  = return $ Token (position st) lx


lexer :: String -> Either String [Token]
lexer s = runAlex s go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TEOF then
        pure [output]
      else (output :) <$> go

-- a função que o Happy vai usar para obter tokens
happyLexer :: (Token -> Alex a) -> Alex a
happyLexer cont = do
  tk <- alexMonadScan
  cont tk

}
