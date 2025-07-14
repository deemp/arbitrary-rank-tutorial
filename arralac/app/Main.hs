module Main where

import Language.Arralac.CLI.Run (runCli)
import Language.Arralac.Prelude.Locale (withCorrectLocale)

main :: IO ()
main = withCorrectLocale runCli
