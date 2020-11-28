{-# LANGUAGE FlexibleContexts #-}
module Parsing.ParsecExample where 

-- This file is from this blog post:
-- https://jsdw.me/posts/haskell-parsec-basics/


-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias Parsec.parse for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text

-- This looks for letters, then spaces, then digits.
-- we then return letters and digits in a tuple.
myParser :: Parsec.Parsec String () (String,String,String)
myParser = do
    letters <- Parsec.many1 Parsec.letter
    Parsec.spaces
    digits <- Parsec.many1 Parsec.digit
    Parsec.spaces
    moreDigits <- Parsec.many1 Parsec.digit
    return (letters,digits, moreDigits)

    
helloOrHowdy :: Parsec.Parsec String () String
helloOrHowdy = do
    first <- Parsec.char 'h'
    rest <- Parsec.string "ello" <|> Parsec.string "owdy"
    return (first:rest)


helloOrHowdy2 :: Parsec.Parsec String () String
helloOrHowdy2 = Parsec.try (Parsec.string "hello") <|> Parsec.string "howdy"


speakTo :: (String -> String) -> IO String
speakTo fSentence = fmap fSentence getLine

-- Usage example.
sayHello :: IO String
sayHello = speakTo (\name -> "Hello, " ++ name ++ "!")
    