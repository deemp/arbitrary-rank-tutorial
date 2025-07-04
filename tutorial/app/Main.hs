{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.STLC.Common ()

-- import Language.STLC.Syntax.Abs (Statement)
-- import Language.STLC.Syntax.Print (printTree)

import Language.STLC.LanguageServer.Main qualified as LS
import Main.Utf8 (withUtf8)

-- main :: IO ()
-- main = withUtf8 do
--   putStrLn $ printTree ("#typecheck x:Int |- \\x -> (\\z -> z) x <= Int -> Int" :: Statement)

-- TODO CLI

main :: IO Int
main = withUtf8 do LS.main
