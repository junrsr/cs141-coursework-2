module Hurtle.Parser where

import Hurtle.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty


myLexeme :: Parser a -> Parser a
myLexeme = L.lexeme spaceConsumer


mySymbol :: String -> Parser String
mySymbol = L.symbol spaceConsumer


integer :: Parser Int
integer = myLexeme L.decimal


floating :: Parser Float
floating = try (myLexeme L.float) <|> fmap fromIntegral integer


commandParser :: Parser HogoCode
commandParser = forwardParser
  <|> backwardParser
  <|> leftParser
  <|> rightParser
  <|> penUpParser
  <|> penDownParser
  <|> homeParser
  <|> clearScreenParser
  <|> repeatParser

forwardParser :: Parser HogoCode
forwardParser = fmap GoForward (mySymbol "forward" *> floating)

backwardParser :: Parser HogoCode
backwardParser = fmap GoBackward (mySymbol "back" *> floating)

leftParser :: Parser HogoCode
leftParser = fmap TurnLeft (mySymbol "left" *> floating)

rightParser :: Parser HogoCode
rightParser = fmap TurnRight (mySymbol "right" *> floating)

penUpParser :: Parser HogoCode
penUpParser = fmap (const PenUp) (mySymbol "penup")

penDownParser :: Parser HogoCode
penDownParser = fmap (const PenDown) (mySymbol "pendown")

homeParser :: Parser HogoCode
homeParser = fmap (const GoHome) (mySymbol "home")

clearScreenParser :: Parser HogoCode
clearScreenParser = fmap (const ClearScreen) (mySymbol "clearscreen")


repeatParser :: Parser HogoCode
repeatParser = do
  _ <- mySymbol "repeat"
  n <- integer
  _ <- mySymbol "["
  commands <- many commandParser
  _ <- mySymbol "]"
  pure $ Repeat n commands

parseHogo :: Parser HogoProgram
parseHogo = between spaceConsumer eof (many commandParser)
