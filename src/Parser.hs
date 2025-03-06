{-# LANGUAGE RecordWildCards #-}

module Parser
  -- ( module Parser
  -- , module Text.Megaparsec
  -- )
where

import Control.Applicative
import Control.Monad
import Data.Void

import Text.Megaparsec as P
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- space consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "#")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed (return ()) integer

type Variable = String

pVariable :: Parser Variable
pVariable = lexeme
  ((:) <$> (char '$') <*> P.many alphaNumChar <?> "variable")

pString :: Parser String
pString = lexeme
  ((:) <$> alphaNumChar <*> P.many alphaNumChar <?> "variable")

data Command = Command
  { commandString :: String
  , assignments :: [Assignment]
  , constraints   :: [Constraint]
  } deriving (Eq, Show)

pCommand :: Parser Command
pCommand = do
  commandString <- pCommandString
  assignments <- pAssignments
  constraints <- return []
  void (char ';')
  return Command {..}

pCommandString :: Parser String
pCommandString = manyTill
  anySingle $
  lookAhead $ choice $ char <$>
  ['<', '{', ';']

data Assignment = Assignment
  { var :: Variable
  , value :: StringExpr
  } deriving (Eq, Show)

pAssignment :: Parser Assignment
pAssignment = do
  var <- pVariable
  void (symbol ":=")
  value <- pStringExpr
  return Assignment {..}

pAssignments :: Parser [Assignment]
pAssignments = between
  (lexeme $ char '<') (lexeme $ char '>') inner
  where
    inner :: Parser [Assignment]
    inner = do
      first <- pAssignment <?> "at least one assignment"
      rest <- manyTill
              (do
                  void (symbol ",")
                  a <- pAssignment
                  return a)
              (lookAhead (char '>'))
      return $ first : rest

data StringExpr
  = Raw String
  | Var Variable
  | ExtSet Variable String
  | ExtAdd Variable String
  | ExtReplace Variable (String, String)
  | ExtStrip Variable
  -- Append?
  deriving (Eq, Show)

pStringExpr :: Parser StringExpr
pStringExpr = choice $ try <$>
  [ do
      lit <- lexeme stringLiteral
      return $ Raw lit
  , do
      var <- pVariable
      void (symbol ".=")
      val <- stringLiteral
      return $ ExtSet var val
  , do
      var <- pVariable
      void (symbol ".+=")
      val <- stringLiteral
      return $ ExtAdd var val
  , do
      var <- pVariable
      void (symbol ".=/")
      search <- stringLiteral
      replace <- stringLiteral
      return $ ExtReplace var (search, replace)
  , do
      var <- pVariable
      void (symbol ".-")
      return $ ExtStrip var
  , do
      var <- pVariable
      return $ Var var
  ]

data Constraint
  = UserConfirmation
  | FileExists String
  | DirectoryExists String
  deriving (Eq, Show)
