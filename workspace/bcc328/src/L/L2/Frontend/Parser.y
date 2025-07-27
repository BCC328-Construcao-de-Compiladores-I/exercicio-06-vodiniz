
{
module L.L2.Frontend.Parser (l2Parser, parserTest, parser) where

import L.L2.Frontend.Lexer
  ( Alex
  , runAlex
  , alexMonadScan
  , alexError
  , Token(..)
  , Lexeme(..)
  , happyLexer
  )
import L.L2.Frontend.Syntax (L2(..), S2(..), E2(..))
import Utils.Var  (Var(..))
import Utils.Value(Value(..))
}

%name parser L2
%tokentype { Token }
%monad      {Alex}{(>>=)}{return}
%error      { parseError }
%lexer      { happyLexer }{ Token _ TEOF }

%token
  num     { Token _ (TNumber $$) }
  str     { Token _ (TString $$) }
  id      { Token _ (TIdent $$) }
  ":="    { Token _ TAssign }
  ";"     { Token _ TSemi   }
  "("     { Token _ TLParen }
  ")"     { Token _ TRParen }
  "+"     { Token _ TPlus   }
  "-"     { Token _ TMinus  }
  "*"     { Token _ TTimes  }
  "/"     { Token _ TDiv    }
  read    { Token _ TRead   }
  print   { Token _ TPrint  }
  ","     { Token _ TComma  }
  def   { Token _ TDef }
  in    { Token _ TIn }
  end     { Token _ TEnd}

%left "+" "-"
%left "*" "/"
%nonassoc "("

%%

L2 :                                        { L2 []} 
   | S2 L2                                  { let L2 xs = $2 in L2 ( $1 : xs ) }
   ;

S2 : def id ":=" E2 in L2 end         {let (L2 inner) = $6 in Def (Var $2) $4 inner}
   | id ":=" E2 ";"                         {LAssign (Var $1) $3}
   | read "(" str "," id ")" ";"            {LRead $3 (Var $5)}
   | print "(" E2 ")" ";"                   {LPrint $3}
   ;

E2 : E2 "+" E2                              {LAdd $1 $3}
   | E2 "-" E2                              {LMinus $1 $3}
   | E2 "*" E2                              {LMul $1 $3}
   | E2 "/" E2                              {LDiv $1 $3}
   | "(" E2 ")"                             {$2}
   | num                                    {LVal (VInt $1)}
   | str                                    {LVal (VStr $1)}
   | id                                     {LVar (Var $1)}    
   ;
{

parserTest :: String -> IO ()
parserTest s = do
  r <- l2Parser s 
  print r 

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)


l2Parser :: String -> IO (Either String L2)
l2Parser content = do 
  pure $ runAlex content parser
}


