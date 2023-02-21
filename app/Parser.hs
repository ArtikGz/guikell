module Parser where

import Control.Applicative
import Control.Monad
import Data.Either
import DefwDefinition
import Error
import Utils
import Data.Char

type ParseError = String

newtype Parser a = Parser
  { run :: String -> Either ParseError (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Right (rest, x) -> Right (rest, f x)
      Left err -> Left err

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

instance MonadPlus Parser

withError :: (String -> String) -> Parser a -> Parser a
withError onError (Parser p) = Parser f
  where
    f input =
      case p input of
        Left _ -> Left $ onError input
        r -> r

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

-- TODO remove spaces
defwWindow :: Parser DefwToken
defwWindow = DefwWindow <$> (word "win" *> spaces *> defwCommands <* spaces <* word "end")

defwCommands :: Parser [DefwToken]
defwCommands = some defwCommand

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
defwDraw = (\_ -> DefwTitle "TODO") <$> (word "draw" *> spaces *> some defwDrawElement <* spaces <* word "end")

defwDrawElement = undefined

{-
defwDrawElement :: Parser DefwToken
defwDrawElement = element *> spaces *> defwData <* spaces <* word "end"
  where
    element = (msum . map word) allowed
    allowed = ["rect", "text"]

defwData :: Parser DefwData
defwData = msum rules
  where
    rules =
      [ word "at" *> (defwNumber, defwNumber),
        word "sized" *> (defwNumber <*> defwNumber)
      ]
    toDefwData cmd args = case cmd of
      "sized" -> DefwSized (tuple2 args)
      "at" -> DefwAt (tuple2  args)
-}
defwString :: Parser String
defwString =
  withError
    (expectedNull "string")
    (char '"' *> sequence' (/= '"') <* char '"')

defwNumber :: Parser String
defwNumber = sequence' isDigit

space :: Parser Char
space =
  withError
    (expectedNull "space")
    (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')

spaces :: Parser String
spaces = some space
