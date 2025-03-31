{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.ModularSTLC.Common ()
import Language.ModularSTLC.Syntax.Abs (Statement)
import Language.ModularSTLC.Syntax.Print (printTree)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
  putStrLn $ printTree ("#typecheck x:Int |- \\x. (\\z.z) x <= Int -> Int" :: Statement)
