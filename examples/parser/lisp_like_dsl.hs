module DSL where

import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Map (Map, (!), fromList)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid

-- Basic Expression definitions
type Id = String

data OperatorType = Add | Multiply
  deriving (Show, Eq)

operatorLiterals :: [(Char, OperatorType)]
operatorLiterals = [('+', Add), ('*', Multiply)]

operatorFunctions :: [(Int -> Int -> Int, OperatorType)]
operatorFunctions = [((+), Add), ((*), Multiply)]

seek :: Eq a => a -> [(b, a)] -> Maybe b
seek x [] = Nothing
seek x ((y, x'):ys) = if x == x' then Just y else seek x ys

getLiteral :: OperatorType -> Maybe Char
getLiteral o = seek o operatorLiterals

getOp :: OperatorType -> Maybe (Int -> Int -> Int)
getOp o = seek o operatorFunctions

data Expression =
  Constant Int
  | Variable Id
  | BinaryOperation OperatorType (Expression, Expression)
  deriving (Show)

constructOp :: Expression -> OperatorType -> Expression -> Expression
constructOp e o e' = BinaryOperation o (e, e')

-- Parser support
data Parser a = Parser { runParser :: String -> Maybe (a, String) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = Parser (fmap (first f) <$> runParser parser)

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  f <*> x = Parser (runAfter . runParser f)
    where runAfter Nothing = Nothing
          runAfter (Just (f', s')) = first f' <$> runParser x s'

-- This is one possible monoidal instance for applicative
-- reflecting here that if parser a fails, then try parser b
instance Alternative Parser where
  empty = Parser (const Nothing)
  -- (<|>) :: Parser a -> Parser a -> Parser a
  x <|> y = Parser (either (runParser x) (runParser y))
    where either f g x = f x <|> g x

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser f
  where f [] = Nothing
        f (x:xs) = if predicate x then Just (x, xs) else Nothing

char :: Char -> Parser Char
char x = satisfy (== x)

oneOrMore :: Parser a -> Parser [a]
oneOrMore x = (:) <$> x <*> zeroOrMore x

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore x = oneOrMore x <|> pure []

-- Monoid instance for Parser of a Monoid
instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
  -- (<>) :: Monoid a => Parser a -> Parser a -> Parser a
  f `mappend` g = fmap (<>) f <*> g

idParser :: Parser String
idParser = (pure <$> satisfy isAlpha) <> zeroOrMore (satisfy isAlphaNum)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

intParser :: Parser Int
intParser = Parser f
  where f [] = Nothing
        f xs
          | null literal = Nothing
          | otherwise = Just (read literal, remainder)
          where (literal, remainder) = span isDigit xs


opParser :: Parser OperatorType
opParser = spaces *> foldl (<|>) empty (map (\(c, o) -> const o <$> char c) operatorLiterals) <* spaces

expressionParser :: Parser Expression
expressionParser = spaces *> (operatorParser <|> reducableParser) <* spaces
  where 
    varParser = Variable <$> idParser
    constantParser = Constant <$> intParser
    reducableParser = varParser <|> constantParser <|> (char '(' *> expressionParser <* char ')')
    operatorParser = liftA3 constructOp reducableParser opParser reducableParser

-- Pretty printer support
prettyPrint :: Expression -> String
prettyPrint (Constant n) = show n
prettyPrint (Variable v) = v
prettyPrint (BinaryOperation o (x, y)) = "(" ++ prettyPrint x ++
                                          [fromMaybe '?' (getLiteral o)]
                                          ++ prettyPrint y ++ ")"

-- Evaluation
type Environment = Map String Int

eval :: Environment -> Expression -> Int
eval _ (Constant n) = n
eval env (Variable x) = env ! x
eval env (BinaryOperation o (x, y)) = fromMaybe const (getOp o) (eval env x) (eval env y)

-- Optimization

main = do 
  print $ runParser expressionParser "  (   (  2 * 3) + (4 + x))";
  print $ runParser expressionParser "x + y";
  print $ prettyPrint . fst <$> runParser expressionParser "  (   (  2 * 3) + (4 + x))";
  print $ eval (fromList [("x", 3)]) . fst <$> runParser expressionParser "  (   (  2 * 3) + (4 + x))";
