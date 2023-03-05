module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Either
import           DefwDefinition
import           Error
import           Utils

type ParseError = String

type ParseResult a = Either ParseError (String, a)

newtype Parser a = Parser
  { run :: String -> ParseResult a
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (rest, x) <- p input
    return (rest, f x)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (input, a)

  (Parser pa) <*> (Parser pb) = Parser f
    where
      f input = do
        (rest, x) <- pa input
        (rest', y) <- pb rest
        pure (rest', x y)

instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty"

  (Parser pa) <|> (Parser pb) = Parser f
    where
      f input
        | isLeft (pa input) = pb input
        | otherwise = pa input

instance Monad Parser where
  (Parser a) >>= pf = Parser f
    where
      f input = case a input of
        Right (rest, result) ->
          let Parser b = pf result
          in b rest
        Left err -> Left err

withError :: (String -> String) -> Parser a -> Parser a
withError onError (Parser p) = Parser f
  where
    f input =
      case p input of
        Left _ -> Left $ onError input
        r      -> r

sequence' :: (Char -> Bool) -> Parser String
sequence' predicate = Parser f
  where
    f = Right . reverse' . span predicate

char :: Char -> Parser Char
char c = Parser f
  where
    f input@(x : xs)
      | x == c = Right (xs, x)
      | otherwise = Left $ expectedError "character" [c] [x]
    f [] = Left $ expectedError "character" [c] []

word :: String -> Parser String
word expected =
  withError
    (expectedError "word" expected)
    (traverse char expected)

defwWindow :: Parser DefwToken
defwWindow = DefwWindow <$> (word "win" *> spaces *> defwCommands <* word "end")

defwCommands :: Parser [DefwToken]
defwCommands = some (defwCommand <* spaces)

defwCommand :: Parser DefwToken
defwCommand = defwTitle <|> defwDraw

defwTitle :: Parser DefwToken
defwTitle = DefwTitle <$> checked (word "title" *> spaces *> defwString)
  where
    checked (Parser p) = Parser $ \i -> case p i of
      Right (rest, result) ->
        if null result
          then Left "Title cannot be empty!"
          else Right (rest, result)
      Left err -> Left err

defwDraw :: Parser DefwToken
defwDraw = DefwDraw <$>
  (word "draw" *> spaces *> word "->" *> spaces *> char '(' *> many defwArgument <* char ')' <* spaces)
  <*> ((some defwDrawElement) <* word "end")

defwDrawElement :: Parser DefwToken
defwDrawElement = DefwCommand <$> element <*> (spaces *> many (defwData <* spaces))
  where
    element = (asum . map word) allowed
    allowed = ["rect", "text"]

defwArgument :: Parser DefwArgument
defwArgument = (,) <$> (defwString <* space) <*> defwString

defwData :: Parser DefwData
defwData = asum rules
  where
    rules =
      [ toDefwData <$> (word "at" <* space) <*> doubleNum,
        toDefwData <$> (word "sized" <* space) <*> doubleNum
      ]
    doubleNum = (,) <$> (defwNumber <* space) <*> defwNumber
    toDefwData cmd args = case cmd of
      "sized" -> DefwSized args
      "at"    -> DefwAt args

defwString :: Parser String
defwString =
  withError
    (expectedNull "string")
    (char '\"' *> sequence' (/= '\"') <* char '\"')

defwNumber :: Parser Int
defwNumber = read <$> sequence' isDigit

space :: Parser Char
space =
  withError
    (expectedNull "space")
    (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')

spaces :: Parser String
spaces = some space
