#!/usr/bin/env stack
{- stack --resolver nightly-2020-09-25 runhaskell --package rio  --package megaparsec --package replace-megaparsec -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import RIO.State
import RIO.Lens
import RIO hiding (many)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import qualified RIO.Text.Lazy as LT
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Replace.Megaparsec
import Text.Megaparsec.Char.Lexer

bracevar :: ParsecT Void Text m Text
bracevar = between (string "{{") (string "}}") (T.pack <$> many (alphaNumChar <|> spaceChar))

addClozeNumbers :: Text -> State Int Text
addClozeNumbers x = do
        i <- get
        let i' = i+1
        put i'
        pure $ "{{c" <> (T.pack $ show i') <> "::" <> x <> "}}"

thing :: Text -> [Text]
thing = fmap (view _2) . rights . splitCap (match bracevar)

main :: IO ()
main = print $ thing "{{foo.}}"

genPhrase :: Text -> Text
genPhrase = flip evalState 0 . streamEditT bracevar addClozeNumbers
