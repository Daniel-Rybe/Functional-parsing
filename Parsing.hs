{-# LANGUAGE LambdaCase #-}

-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Edited by Daniel-Rybe

module Parsing
    ( module Parsing
    , module Control.Applicative
    ) where

import           Control.Applicative
import           Control.Monad                  ( join )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isDigit
                                                , isLower
                                                , isSpace
                                                , isUpper
                                                )
import           Data.Functor.Identity          ( Identity(runIdentity) )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )

newtype ParserT m a = P (String -> m [(a, String)])

type Parser = ParserT Identity

parseT :: ParserT m a -> String -> m [(a, String)]
parseT (P p) = p

parse :: Parser a -> String -> [(a, String)]
parse (P p) = runIdentity . p

item :: Monad m => ParserT m Char
item = P $ \case
    []       -> return []
    (x : xs) -> return [(x, xs)]

-- Utils

joinMLML :: Monad m => m [m [a]] -> m [a]
joinMLML mlmlb = do
    lmlb <- mlmlb
    if null lmlb then return [] else foldl1 mConcat lmlb

mConcat :: Monad m => m [a] -> m [a] -> m [a]
mConcat mas mbs = do
    as <- mas
    bs <- mbs
    return $ as ++ bs

-- Sequencing parsers

instance Monad m => Functor (ParserT m) where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P $ (fmap . fmap) (applyToFst g) . parseT p
        where applyToFst f (a, b) = (f a, b)

instance Monad m => Applicative (ParserT m) where
    -- pure :: a -> Parser a
    pure v = P $ \inp -> return [(v, inp)]

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P $ \inp ->
        let mlgout = parseT pg inp
            mlmlb  = map (\(g, out) -> parseT (g <$> px) out) <$> mlgout
        in  joinMLML mlmlb

instance Monad m => Monad (ParserT m) where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \inp ->
        let mlaout = parseT p inp
            mlmlb  = map (\(a, out) -> parseT (f a) out) <$> mlaout
        in  joinMLML mlmlb

-- Making choices

instance Monad m => Alternative (ParserT m) where
    -- empty :: Parser a
    empty = P $ const $ return []

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \inp -> do
        res <- parseT p inp
        case res of
            []  -> parseT q inp
            res -> return res

-- Parser concatenation
(<:>) :: Parser a -> Parser a -> Parser a
p <:> q = P $ \inp -> mConcat (parseT p inp) (parseT q inp)

-- Derived primitives

sat :: Monad m => (Char -> Bool) -> ParserT m Char
sat p = do
    x <- item
    if p x then return x else empty

digit :: Monad m => ParserT m Char
digit = sat isDigit

lower :: Monad m => ParserT m Char
lower = sat isLower

upper :: Monad m => ParserT m Char
upper = sat isUpper

letter :: Monad m => ParserT m Char
letter = sat isAlpha

alphanum :: Monad m => ParserT m Char
alphanum = sat isAlphaNum

char :: Monad m => Char -> ParserT m Char
char x = sat (== x)

string :: Monad m => String -> ParserT m String
string []       = return []
string (x : xs) = do
    char x
    string xs
    return (x : xs)

ident :: Monad m => ParserT m String
ident = do
    x  <- lower
    xs <- many alphanum
    return (x : xs)

nat :: Monad m => ParserT m Int
nat = do
    xs <- some digit
    return (read xs)

int :: Monad m => ParserT m Int
int =
    do
        char '-'
        n <- nat
        return (-n)
    <|> nat

-- Handling spacing

space :: Monad m => ParserT m ()
space = do
    many (sat isSpace)
    return ()

token :: Monad m => ParserT m a -> ParserT m a
token p = do
    space
    v <- p
    space
    return v

identifier :: Monad m => ParserT m String
identifier = token ident

natural :: Monad m => ParserT m Int
natural = token nat

integer :: Monad m => ParserT m Int
integer = token int

symbol :: Monad m => String -> ParserT m String
symbol xs = token (string xs)

-- Repeating parsers

repeatP :: Monad m => Integer -> ParserT m a -> ParserT m [a]
repeatP n p = if n > 0
    then do
        a <- p
        (a :) <$> repeatP (n - 1) p
    else return []
