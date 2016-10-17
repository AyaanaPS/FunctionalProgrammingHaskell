--
-- S-expression parser.
--

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA  Bool
  | IntA   Integer
  | FloatA Double
  | IdA    String  -- identifier
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  expE <- option "0" parseFloatExp
  return (read (sign ++ digits ++ "." ++ f ++ "e" ++ expE) :: Double)
  <?> "floating-point number"

-- This helper function finds the exponent if one exists.
parseFloatExp :: Parser String
parseFloatExp = do
  oneOf "eE"
  signExp <- option "" (string "-" <|> string "+")
  expVal <- many1 digit
  return (signExp ++ expVal)

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

parseString :: Parser String
parseString = do
  char '\"'
  stringRead <- many (noneOf "\"")
  char '\"'
  return stringRead

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return .StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

{-
  Question 3: We do not need to have a try because there is no need
  to back trace for this part. This is because a parenthesis/bracket is
  only one character.
-}

-- This calls parseListHelper based on the type of bracket.
parseList :: Parser [Sexpr]
parseList = 
  (parseListHelper '(' ')')
  <|> (parseListHelper '[' ']')
  <|> (parseListHelper '{' '}')

-- Parse a list of S-expressions, delimited by the given type of bracket,
-- separated by whitespace/comments.
parseListHelper :: Char -> Char -> Parser [Sexpr]
parseListHelper a b = do
  char a
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char b
  return ss
  <?> "list of S-expressions"

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = char '\'' >> parseSexpr
  <?> "quoted S-expression"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> (parseQuote >>= return . (\x -> ListS [AtomS (IdA "quote"), x]))
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"
