module Hurtle.Parser where

import Hurtle.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Text.Megaparsec.Char.Lexer as L

-- | Consumes whitespace and Haskell-style single-line comments.
-- It uses 'space1' to consume one or more spaces, 'L.skipLineComment' to
-- ignore comments starting with ";", and 'empty' since we do not have block comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

-- | Wraps a parser to consume any trailing whitespace or comments, ensuring
-- that the parser does not fail due to unexpected spaces or comments after the relevant token.
myLexeme :: Parser a -> Parser a
myLexeme = L.lexeme spaceConsumer

-- | Parses a specific string while consuming trailing space using the 'myLexeme' function,
-- helping to keep the grammar clean by not requiring explicit whitespace handling after each token.
mySymbol :: String -> Parser String
mySymbol = L.symbol spaceConsumer

-- | Parses an integer, consuming any trailing whitespace or comments.
-- This is useful for parsing numeric values like the repetition count in 'repeat' blocks.
integer :: Parser Int
integer = myLexeme L.decimal

-- | Parses a floating point number, also allowing integers by converting them to Float.
-- This flexibility is important for commands that accept numbers as they can be specified
-- as either integers or floats.
floating :: Parser Float
floating = try (myLexeme L.float) <|> fmap fromIntegral integer

-- | Parses individual Hogo commands by trying each known command.
-- The use of 'choice' allows the parser to attempt each command in turn until one succeeds.
-- This is where the actual syntax of the Hogo language gets defined.
commandParser :: Parser HogoCode
commandParser = choice
  [ GoForward <$> (mySymbol "forward" *> floating)
  , GoBackward <$> (mySymbol "back" *> floating)
  , TurnLeft <$> (mySymbol "left" *> floating)
  , TurnRight <$> (mySymbol "right" *> floating)
  , PenUp <$ mySymbol "penup"
  , PenDown <$ mySymbol "pendown"
  , GoHome <$ mySymbol "home"
  , ClearScreen <$ mySymbol "clearscreen"
  , try repeatParser -- 'try' allows for backtracking, which is crucial for correctly parsing 'repeat' blocks.
  ]

-- | Parses the 'repeat' command, which includes an integer indicating the number of repetitions
-- and a block of commands to be repeated, delimited by "[" and "]".
-- This parser demonstrates handling of nested structures and control flow constructs in the language.
repeatParser :: Parser HogoCode
repeatParser = do
  _ <- mySymbol "repeat" -- Parses the 'repeat' keyword.
  n <- integer         -- Parses the number of times the commands should be repeated.
  _ <- mySymbol "["      -- Parses the opening bracket of the command block.
  commands <- many commandParser -- Recursively parses multiple commands within the block.
  _ <- mySymbol "]"      -- Parses the closing bracket of the command block.
  return $ Repeat n commands -- Constructs the 'Repeat' instruction with the parsed count and commands.

-- | The main parser for the Hogo language. It uses 'between' to strip leading and trailing
-- whitespace and comments, ensuring that the entire input is considered. The 'many commandParser'
-- part uses the 'commandParser' to parse a series of Hogo commands, building up a list of 'HogoCode' values.
-- This is the entry point for parsing a Hogo program from a string.
parseHogo :: Parser HogoProgram
parseHogo = between spaceConsumer eof (many commandParser)
