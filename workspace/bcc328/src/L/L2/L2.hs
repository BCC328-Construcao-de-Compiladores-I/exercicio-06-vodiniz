import L.L2.Frontend.Lexer
import L.L2.Frontend.Parser
import L.L2.Frontend.Syntax
import L.L2.Frontend.TypeCheck
import L.L2.Interpreter.Interp
import Utils.Pretty

import L.L2.Backend.V1Codegen (v1Codegen)
import L.L2.Frontend.TypeCheck (typeCheck)
import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    lexerOnly file
  [Parser file] ->
    parserOnly file
  [Interpret file] ->
    interpret file
  [VM file] ->
    v1Compiler file
  [C file] ->
    cCompiler file
  _ -> helpMessage

-- Implement the function to do lexical analysis for L2 programs and outputs the tokens

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  content <- readFile file
  case lexer content of
    Left err -> print err
    Right tokens -> mapM_ printToken tokens

-- Implement the function to do syntax analysis for L2 programs and outputs the syntax tree
parserOnly :: FilePath -> IO ()
parserOnly file = do
  -- reading file
  content <- readFile file
  -- Parsing
  result <- L.L2.Frontend.Parser.l2Parser content
  case result of -- AQUI aplica `l2Parser` ao `content`
    Left err -> print ("Parse error: " ++ err)
    Right ast -> print (ppr ast)

-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program
--

-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program
interpret :: FilePath -> IO ()
interpret file = do
  content <- readFile file
  result <- l2Parser content
  case result of
    Left perr -> print ("Parse error: " ++ perr)
    Right ast -> do
      let result = typeCheck ast
      case result of
        Left terr -> print ("Type error: " ++ terr)
        Right ast' -> do
          result <- interp ast'
          case result of
            Left ierr -> print ("Runtime error: " ++ ierr)
            Right _ -> return ()

-- \| Imprime um único Token no formato desejado
printToken :: Token -> IO ()
printToken (Token (line, col) lex) = case lex of
  TIdent s ->
    putStrLn $
      "Identificador "
        ++ s
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TAssign ->
    putStrLn $
      "Atribuição :="
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TLParen ->
    putStrLn $
      "Parêntesis ("
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TRParen ->
    putStrLn $
      "Parêntesis )"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TPlus ->
    putStrLn $
      "Soma +"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TMinus ->
    putStrLn $
      "Menos -"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TTimes ->
    putStrLn $
      "Vezes *"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TNumber n ->
    putStrLn $
      "Número "
        ++ show n
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TRead ->
    putStrLn $
      "Palavra reservada read"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TPrint ->
    putStrLn $
      "Palavra reservada print"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TSemi ->
    putStrLn $
      "Ponto e vírgula ;"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TComma ->
    putStrLn $
      "Vírgula ,"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TString s ->
    putStrLn $
      "Literal de string \""
        ++ s
        ++ "\""
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TDef ->
    putStrLn $
      "Def block"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TEnd ->
    putStrLn $
      "End def block"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TIn ->
    putStrLn $
      "In"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col
  TEOF ->
    putStrLn $
      "Fim de arquivo (EOF)"
        ++ " Linha:"
        ++ show line
        ++ " Coluna:"
        ++ show col

--- Implement the whole compiler pipeline: lexical, syntax and semantic analysis and then generate v1 instructions from the program.

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  content <- readFile file
  result <- l2Parser content
  case result of
    Left perr -> print ("Parse error: " ++ perr)
    Right ast -> do
      let result = typeCheck ast
      case result of
        Left terr -> print ("Type error: " ++ terr)
        Right ast' -> do
          let code = v1Codegen ast'
          let doc = pretty code
          let outfile = replaceExtension file ".v1"
          writeFile outfile doc
          print ("Generated v1 code to :" ++ outfile)

-- Implement the whole executable compiler, using C source and GCC.

cCompiler :: FilePath -> IO ()
cCompiler file = error "Not implemented!"

-- help message

helpMessage :: IO ()
helpMessage =
  putStrLn $
    unlines
      [ "L2 language"
      , "Usage: l2 [--lexer-only | --parse-only | --interpret | --help]"
      , "--lexer-only: does the lexical analysis of the input program."
      , "--parse-only: does the syntax analysis of the input program."
      , "--interpret: does the syntax and semantic analysis and interpret the input program."
      , "--v1: does the syntax and semantic analysis and then generates V1 code."
      , "--c: does the syntax and semantic analysis, generates C code and uses GCC to generate an executable."
      , "--help: prints this help message."
      ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Interpret FilePath
  | VM FilePath
  | C FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    ("--v1" : arg : _) -> [VM arg]
    ("--c" : arg : _) -> [C arg]
    _ -> [Help]
