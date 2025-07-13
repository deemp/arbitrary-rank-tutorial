{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Arralac.LanguageServer.Run qualified as LS
import Main.Utf8 (withUtf8)

-- TODO CLI

main :: IO Int
main = withUtf8 do LS.main
