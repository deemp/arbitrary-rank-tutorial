module Language.Arralac.Utils.Debug where

import Control.Monad (when)
import GHC.Stack (callStack, prettyCallStack)
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types
import Prettyprinter
import Prettyprinter.Util (putDocW)

-- =====================
-- [Debugging utilities]
-- =====================

debug :: (CtxDebug, CtxPrettyVerbosity) => Doc a -> [Doc a] -> IO ()
debug label xs = when ?debug do
  putDocW
    1000
    ( vsep
        [ "[" <> label <> "]"
        , foldMap (\x -> "$ " <> x <> line) xs
        , pretty' (prettyCallStack callStack)
        , line
        ]
    )

debug' :: (CtxDebug, CtxPrettyVerbosity) => Doc ann -> [(Doc ann, Doc ann)] -> IO ()
debug' label xs = debug label (prettyVarVal <$> xs)
 where
  prettyVarVal :: (Doc ann, Doc ann) -> Doc ann
  prettyVarVal (var, val) = var <> ":" <> line <> indent 4 val
