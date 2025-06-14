{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Source: https://github.com/haskell/cabal/issues/6726#issuecomment-918663262

-- | Custom Setup that runs bnfc to generate the language sub-libraries
-- for the parsers included in Ogma.
module Main (main) where

import Control.Exception (Handler (..), SomeException, catches, displayException, evaluate)
import Data.List (intercalate)
import Distribution.Simple (defaultMainWithHooks, hookedPrograms, postConf, preBuild, simpleUserHooks)
import Distribution.Simple.Program (Program (..), findProgramVersion, simpleProgram)
import Main.Utf8 (withUtf8)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO.CodePage (withCP65001)
import System.Process (callCommand)

withCorrectLocale :: IO a -> IO a
withCorrectLocale act = do
  let withCorrectLocale' = withCP65001 . withUtf8
  withCorrectLocale' act
    `catches` [ Handler $ \(x :: ExitCode) -> exitWith x
              , Handler $ \(x :: SomeException) ->
                  withCorrectLocale' do
                    putStrLn (displayException x)
                    exitFailure
              ]

-- | Run BNFC, happy, and alex on the grammar before the actual build step.
--
-- All options for bnfc are hard-coded here.
main :: IO ()
main =
  withCorrectLocale $
    defaultMainWithHooks $
      simpleUserHooks
        { hookedPrograms = [bnfcProgram]
        , postConf = \args flags packageDesc localBuildInfo -> do
            let
              -- See the details on the command form in https://github.com/objectionary/eo-phi-normalizer/issues/347#issuecomment-2117097070
              command =
                intercalate
                  "; "
                  [ "set -ex"
                  , "bnfc --haskell --functor --text -d -p Language.STLC --generic -o src/ grammar/STLC/Syntax.cf"
                  , "cd src/Language/STLC/Syntax"
                  , "alex Lex.x"
                  , "happy --ghc Par.y"
                  , "rm -f {ErrM,Skel,Test}.hs *.bak"
                  , "true"
                  ]

              fullCommand = "bash -c ' " <> command <> " '"

            putStrLn fullCommand

            _ <- callCommand fullCommand

            postConf simpleUserHooks args flags packageDesc localBuildInfo
        }

-- | NOTE: This should be in Cabal.Distribution.Simple.Program.Builtin.
bnfcProgram :: Program
bnfcProgram =
  (simpleProgram "bnfc")
    { programFindVersion = findProgramVersion "--version" id
    }
