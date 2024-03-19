module Hurtle.Parser where

import Hurtle.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

{-|
  @RETURN a parser which removes whitespace and comments

  this method defines a parser which removes all whitespace and comments. it does this by utilising the space method in the Text.Megaparsec.Char.Lexer module,
  which produces a parser which can remove whitespace. It takes in 3 variables: a parser for space characters, a parser for comments and a parser for multiline
  comments. In regards to the space character parser, I'm using space1 to skip over one or more white space characters we encounter in the program, such as space
  between keywords or space between newlines, as these don't need to be considered when parsing. In regards to the comments, Text.Megaparsec.Char.Lexer also has
  another method called skipLineComment, which (when it encounters the ; symbol given to it), skips over the rest of the line, returning the new parser. since the
  hogo code doesn't take in any multiline comments, i can just leave that section as empty. with these given values, the L.space method produces a general case
  parser which is able to remove all whitespace and comments.
-}
removals :: Parser ()
removals = L.space space1 (L.skipLineComment ";") empty -- removes whitespace from parser


{-|
  @PARAM the lexeme parser we want to use 
  @RETURN the same parser stripped of whitespace and comments

  this method utilises the removals parser we created earlier. it takes in a given lexeme and then immadiately applies the removals method to remove any whitespace
  and comments, allowing for only non-redundant lexemes to end up being parsed. In this particular program, L.lexeme refers to integer and float values we may
  encounter whilst parsing.

-}
myLexeme :: Parser a -> Parser a
myLexeme = L.lexeme removals


{-|
  @PARAM the command we want to parse
  @RETURN the same command stripped of whitespace and comments

  the mySymbol method takes in a given string and applies the removals method ot it. in this program, it takes a given command and then proceeds to remove comments and
  whitespace from it utilising the removals method defined earlier, returning a new string parser as a result. L.symbol refers to the current command that we are parsing
  (such as home or left 90).
-}
mySymbol :: String -> Parser String
mySymbol = L.symbol removals


{-|
  @RETURN a parsed numeric value
  
  this numeric method takes in a given numeric value, such as the distance we want the turtle to move or how much we want the turtle to rotate by. it utilises the monoid <|>
  to define that our numeric value must return one of two types: integer or float. firstly, this method tries to see if it is a floating point value by trying to parse the
  value as if it were a float, calling the predefined myLexeme method. if it can do that, it will return that parsed value. if it can't, it will instead parse the value as
  if it were an integer, and then utilise the fromIntegral method to convert the given value into a float and return that instead. this ensures that this method will always
  return a floating point value (which all numbers can be represented by). Whilst parsing either value, it utilises the myLexeme method outlined earlier to remove redundant
  lexemes.
-}
numeric :: Parser Float
numeric = try (myLexeme L.float) <|> fmap fromIntegral integer


{-|
  @RETURN a parsed integer value

  this method is used to take in an decimal value and parse it by calling the earlier defined myLexeme method. This removes whitespace and comments and, in the process, returns
  a parsed integer. this is mostly used as a helper method for numeric to parse an integer if the method gives one, but it is also utilised in the repeat method when we detail
  the number of iterations we would like to have.
-}
integer :: Parser Int
integer = myLexeme L.decimal


{-|
  @RETURN the appropriate parser to choose based on the given code

  this method takes in hogo code, and uses the <|> monoid to try and match it with any of the pre-defined parsers for each hogo command detailed to us. It first tries to see
  if it can use the forwardParser on it, then attempts the backwardParser, then the rightParser and continues in that fashion until it reaches the repeat parser, where it will
  then attempt to call that method on it. If, even at this stage, it is unable to find an appropriate parser to match, an error will be returned to the user
-}
commandParser :: Parser HogoCode
commandParser = forwardParser -- takes a given piece of hogo code and chooses the appropriate parser based on given keyword
  <|> backwardParser
  <|> leftParser
  <|> rightParser
  <|> penUpParser
  <|> penDownParser
  <|> homeParser
  <|> clearScreenParser
  <|> repeatParser

{-|
  @RETURN the hogo code which matches the command given

  the following methods are all similar, in the sense that they take in a particular piece of code and then maps the given keyword (e.g. forward, back, left etc)
  to a given hogo command, producting the appropriate HogoCode. For the methods which also take in an additional numeric value, the monadic sequencing operator is
  utilised to, after parsing the relevant keyword (e.g. forward), discard the result and return the numeric value associated with it through the use of the earlier
  defined numeric method. This allows us to pair the relevant command (e.g. GoForward, TurnLeft) with its varying unit. For nmmethods without an additional value,
  it first parses the given command. from here, we map the result of parsing to the command, which utilises the const method to ignore all additional input, only
  returning the hogocode. Parsing is still needed to ensure that all unnecessary whitespace and comments are removed, and also to return an error in the event that
  the keyword doesn't match the one given when calling commandParser, informing the parser to try a different method.
-}
forwardParser :: Parser HogoCode
forwardParser = fmap GoForward (mySymbol "forward" >> numeric) -- handles parsing for the forward method -> maps it to GoForward n

backwardParser :: Parser HogoCode
backwardParser = fmap GoBackward (mySymbol "back" >> numeric) -- handles parsing for the back keyword -> maps it to GoBackward n

leftParser :: Parser HogoCode
leftParser = fmap TurnLeft (mySymbol "left" >> numeric) -- handles parsing for the left keyword -> maps it to TurnLeft n
  
rightParser :: Parser HogoCode
rightParser = fmap TurnRight (mySymbol "right" >> numeric) -- handles parsing for the right keyword -> maps it to TurnRight n

penUpParser :: Parser HogoCode
penUpParser = fmap (const PenUp) (mySymbol "penup") -- handles parsing for the penup method -> maps it to PenUp

penDownParser :: Parser HogoCode
penDownParser = fmap (const PenDown) (mySymbol "pendown") -- handles parsing for the pendown method -> maps it to PenDown

homeParser :: Parser HogoCode
homeParser = fmap (const GoHome) (mySymbol "home") -- handles parsing for the home method -> maps it to GoHome

clearScreenParser :: Parser HogoCode
clearScreenParser = fmap (const ClearScreen) (mySymbol "clearscreen") -- handles parsing for clearscreen -> maps it to ClearScreen


{-|
  @RETURN a given list of command(s) repeated n times over

  this method handles the code whenever we encounter the 'repeat' keyword. it first gets the appropriate symbols from the given input ('repeat', the number of
  iterations, the opening and closing symbols and the content between them). from here, it gets a list of the commands between the opening and closing symbols and
  parses them using the pre-defined commandParser method. The Repeat method is then used to repeat this process for the amount of times n given via the code, and
  this repeated code is then returned by repeatParser using the pure method.

-}
repeatParser :: Parser HogoCode
repeatParser = do
  _ <- mySymbol "repeat" -- checks to see if the given code begins with "repeat" to begin this procedure
  n <- integer -- binds the integer following the keyword "repeat" to n
  _ <- mySymbol "[" -- checks to see if there is an opening square bracket to ensure good formatting
  commands <- many commandParser -- binds the list of commands within the opening and closing symbols to 'commands'
  _ <- mySymbol "]" -- checks to see if there is a closing square bracket to ensure good formatting
  pure $ Repeat n commands --returns the list of commands, repeated a given n amount of times


{-|
  @RETURN the parsed version of the hogo code given

  this method actually parses some hogo program to ensure that it is well formed and meets the strucutre of the language. It utilises the 'between' combinator in
  Text.Megaparsec, which takes in 3 values: the opening parser, the closing parser and the content parser. Here, we can define the opening parser to be removals,
  which can immediately remove all leading comments and whitespace before any work is done. 'eof' is also a method in Text.Megaparsec. it denotes the end of an input,
  so it can be used as the closing parser in our program. finally, the many commandParser method utilises the pre-existing commandParser method and the many method
  from Text.Megaparsec, to turn the hogo program into a list which we can apply the commandParser to, enabling that the hogo program itself can be parsed appropriately
  to ensure good structure and formatting.
-}
parseHogo :: Parser HogoProgram
parseHogo = between removals eof (many commandParser)