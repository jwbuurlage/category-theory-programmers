module DSL where

import Control.Applicative as Appl
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Map as Map (Map, (!), fromList, lookup)
import Data.List (partition)
import Data.Foldable (fold)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid
import Data.Set as Set (singleton, union, empty, toList)

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

-- Pretty printer support
prettyPrint :: Expression -> String
prettyPrint (Constant n) = show n
prettyPrint (Variable v) = v
prettyPrint (BinaryOperation o (x, y)) = "(" ++ prettyPrint x ++
                                          [fromMaybe '?' (getLiteral o)]
                                          ++ prettyPrint y ++ ")"

instance Show Expression where
    show = prettyPrint

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

-- Explicit implementations of `some` or `many` for Parser
-- oneOrMore :: Parser a -> Parser [a]
-- oneOrMore x = (:) <$> x <*> zeroOrMore x
--
-- zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore x = oneOrMore x <|> pure []

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser f
  where f [] = Nothing
        f (x:xs) = if predicate x then Just (x, xs) else Nothing

char :: Char -> Parser Char
char x = satisfy (== x)

-- Monoid instance for Parser of a Monoid
instance Monoid m => Monoid (Parser m) where
  mempty = pure mempty
  -- (<>) :: Monoid a => Parser a -> Parser a -> Parser a
  f `mappend` g = liftA2 (<>) f g

idParser :: Parser String
idParser = fmap pure (satisfy isAlpha) <> many (satisfy isAlphaNum)

spaces :: Parser String
spaces = many (satisfy isSpace)

intParser :: Parser Int
intParser = Parser f
  where f [] = Nothing
        f xs
          | null literal = Nothing
          | otherwise = Just (read literal, remainder)
          where (literal, remainder) = span isDigit xs

opParser :: Parser OperatorType
opParser = spaces *> foldl (<|>) Appl.empty (map (\(c, o) -> const o <$> char c) operatorLiterals) <* spaces

expressionParser :: Parser Expression
expressionParser = spaces *> (operatorParser <|> reducableParser) <* spaces
  where
    varParser = Variable <$> idParser
    constantParser = Constant <$> intParser
    reducableParser = varParser <|> constantParser <|> (char '(' *> expressionParser <* char ')')
    operatorParser = liftA3 constructOp reducableParser opParser reducableParser

-- Evaluation
type Environment = Map Id Int

eval :: Environment -> Expression -> Int
eval _ (Constant n) = n
eval env (Variable x) = env ! x
eval env (BinaryOperation o (x, y)) = fromMaybe const (getOp o) (eval env x) (eval env y)

-- Optimization
-- Can maybe use monoid structure to get id en assoc binary op for add and sum, so that we dont repeat it here
-- note that div and minus is not commutative
optimize :: Expression -> Expression
optimize operation@(BinaryOperation Add (e, e')) = optimizeBinaryOperation Add (optimize e, optimize e') 0 (+)
optimize operation@(BinaryOperation Multiply (e, e'))
    | (null e'' || null e''') = Constant 0
    | otherwise = optimizeBinaryOperation Multiply (e'', e''') 1 (*)
    where (e'', e''') = (optimize e, optimize e')
          null (Constant n) = (n == 0)
          null e = False
optimize expr = expr

optimizeBinaryOperation :: OperatorType -> (Expression, Expression) -> Int -> (Int -> Int -> Int) -> Expression
optimizeBinaryOperation op (Constant a, Constant b) neutral combine = Constant (combine a b)
optimizeBinaryOperation op c@(Constant a, Variable x) neutral _
    | a == neutral = Variable x
    | otherwise = BinaryOperation op c
optimizeBinaryOperation op c@(Variable x, Constant a) neutral _
    | a == neutral = Variable x
    | otherwise = BinaryOperation op c
optimizeBinaryOperation op (e, e') _ _ = BinaryOperation op (e, e')

partial :: Environment -> Expression -> Expression
partial env expr@(Variable x) = case Map.lookup x env of
    Nothing -> expr
    Just n -> Constant n
partial env (BinaryOperation op (e, e')) = BinaryOperation op (partial env e, partial env e')
partial _ expr = expr

eval' :: Environment -> Expression -> Expression
eval' env expr = optimize  (partial env expr)

dependencies :: Expression -> [Id]
dependencies expr = toList $ getvars expr
    where getvars (Variable x) = singleton x
          getvars (BinaryOperation _ (e, e')) = getvars e `union` getvars e'
          getvars _ = Set.empty

result :: Expression -> Either String Int
result (Constant n) = Right n
result expr = Left ("Missing variable bindings: " ++ show (dependencies expr))

-- Monad stuff
instance Monad Parser where
  return = pure
  -- (>>=) Parser a -> (a -> Parser b) -> Parser b
  x >>= f = Parser (
    \s -> do
      (x', s') <- runParser x s
      (y, s'')  <- runParser (f x') s'
      return (y, s''))

threeInts :: Parser [Int]
threeInts = do
  x <- parseOneInt
  y <- parseOneInt
  z <- parseOneInt
  return [x, y, z]
  where
    parseOneInt = spaces *> intParser


main = do
  print $ runParser expressionParser "  (   (  2 * 3) + (4 + x))";
  print $ runParser expressionParser "x + y";
  print $ runParser expressionParser "x";
  print $ runParser expressionParser "5";
  print $ prettyPrint . fst <$> runParser expressionParser "  (   (  2 * 3) + (4 + x))";
  print $ eval (fromList [("x", 3)]) . fst <$> runParser expressionParser "  (   (  2 * 3) + (4 + x))";
  let expr = fromMaybe (Constant 1234) (fst <$> runParser expressionParser "(((5 * y) + (0 + x)) + ((1 * 2) + (0 * zoo123)))")
  let env = fromList [("x", 3), ("y", 1)]
  print $ result $ eval' env expr
  print $ runParser threeInts "123 123 123"
