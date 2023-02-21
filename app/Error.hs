module Error where

expectedError :: String -> String -> String -> String
expectedError t e g = "Expected " ++ t ++ " '" ++ e ++ "', but got " ++ (if null g then "EOF" else "'" ++ (head . words $ g) ++ "'") ++ "!"

expectedNull :: String -> String -> String
expectedNull e i =
  if null i
    then "Expected " ++ e ++ ", but got EOF!"
    else "Expected " ++ e ++ ", but got '" ++ i ++ "'!"