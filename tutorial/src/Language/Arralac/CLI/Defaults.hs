module Language.Arralac.CLI.Defaults where

import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types

data Defaults = Defaults
  { settingsSectionName :: FastString
  , solverIterations :: Int
  , prettyVerbosity :: PrettyVerbosity
  }

defaults :: Defaults
defaults =
  Defaults
    { settingsSectionName = "arralac"
    , solverIterations = 10
    , prettyVerbosity = PrettyVerbosity'Detailed
    }