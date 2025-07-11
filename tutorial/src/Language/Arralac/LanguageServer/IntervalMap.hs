module Language.Arralac.LanguageServer.IntervalMap where

import Control.Lens (traversed, (%~), (&), _1)
import Data.IntervalMap.Generic.Strict qualified as IM
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.SynTerm ()
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Typecheck.Pass
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types
import Language.LSP.Protocol.Types (Position (..), Range (..))
import Prettyprinter (Doc)

newtype IMPosition
  = IMPosition {imPosition :: Position}
  deriving newtype (Eq, Ord)

newtype IMRange
  = IMRange {imRange :: Range}
  deriving newtype (Eq)

data SpanInfo
  = SpanInfo'Name Name
  | SpanInfo'ZnType ZnType

goTermVar :: ZnTermVar -> [(SrcSpan, SpanInfo)]
goTermVar var = [(var.varName.nameLoc, SpanInfo'ZnType var.varType)]

goTyVar :: ZnTyVar -> [(SrcSpan, SpanInfo)]
goTyVar var = [(var.varName.nameLoc, SpanInfo'Name var.varName)]

goType :: SynType CompZn -> [(SrcSpan, SpanInfo)]
goType = \case
  SynType'Var _ var -> goTyVar var
  SynType'Concrete _ ty -> [(ty.concreteName.nameLoc, SpanInfo'Name ty.concreteName)]
  -- HLS doesn't provide info on hover over "forall" and "->".
  -- We don't provide this info either.
  SynType'ForAll _ tvs body -> (concat $ goTyVar <$> tvs) <> goType body
  SynType'Fun _ arg res -> goType arg <> goType res

goTerm :: SynTerm CompZn -> [(SrcSpan, SpanInfo)]
goTerm = \case
  SynTerm'Var _ var ->
    goTermVar var
  SynTerm'Lit anno _ ->
    f anno
  SynTerm'App anno fun arg ->
    f anno <> goTerm fun <> goTerm arg
  SynTerm'Lam anno var body ->
    f anno <> goTermVar var <> goTerm body
  SynTerm'ALam anno var ty body ->
    f anno <> goTermVar var <> goType ty <> goTerm body
  SynTerm'Let anno var val body ->
    f anno <> goTermVar var <> goTerm val <> goTerm body
  SynTerm'Ann anno body ty ->
    f anno <> goTerm body <> goType ty
 where
  f ZnAnno{annoSrcLoc, annoType} =
    [(annoSrcLoc, SpanInfo'ZnType annoType)]

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([], [])
partitionWith f (x : xs) =
  case f x of
    Left b -> (b : bs, cs)
    Right c -> (bs, c : cs)
 where
  (bs, cs) = partitionWith f xs

-- All spans must belong to the same file
-- because they're all produced
-- from a single AST
toIntervalMap :: SynTerm CompZn -> IM.IntervalMap IMRange SpanInfo
toIntervalMap term = IM.fromList spanInfo
 where
  isRealSrcSpan = \case
    ( RealSrcSpan
        RealSrcSpan'
          { srcSpanSLine
          , srcSpanSCol
          , srcSpanELine
          , srcSpanECol
          }
      , info
      ) ->
        Right
          ( IMRange
              Range
                { _start =
                    Position
                      { _line = fromIntegral srcSpanSLine
                      , _character = fromIntegral srcSpanSCol
                      }
                , _end =
                    Position
                      { _line = fromIntegral srcSpanELine
                      , _character = fromIntegral srcSpanECol
                      }
                }
          , info
          )
    (UnhelpfulSpan span', info) -> Left (span', info)
  spanInfo = snd (partitionWith isRealSrcSpan (goTerm term))

-- | Find `IMRange` that contains the 'IMPosition'.
lookupAtIMPosition :: IMPosition -> IM.IntervalMap IMRange a -> Maybe (IMRange, a)
lookupAtIMPosition pos mp =
  case mbRange of
    Nothing -> Nothing
    Just (range, _) ->
      if IM.inside pos range
        then mbRange
        else Nothing
 where
  pos' = pos.imPosition
  range' = IMRange (Range{_start = pos', _end = pos'})
  mbRange = IM.lookupLE range' mp

toRealSrcSpan :: FastString -> IMRange -> RealSrcSpan
toRealSrcSpan filePath (IMRange r) =
  RealSrcSpan'
    { srcSpanFile = filePath
    , srcSpanSLine = fromIntegral r._start._line
    , srcSpanSCol = fromIntegral r._start._character
    , srcSpanELine = fromIntegral r._end._line
    , srcSpanECol = fromIntegral r._end._character
    }

prettyIM :: (IPrettyVerbosity, Pretty' a) => FastString -> IM.IntervalMap IMRange a -> Doc ann
prettyIM filePath mp = pretty' mp'
 where
  mp' = IM.toAscList mp & traversed . _1 %~ toRealSrcSpan filePath

instance Ord IMRange where
  IMRange x <= IMRange y =
    x._start < y._start
      || x._start == y._start && x._end >= y._end

instance IM.Interval IMRange IMPosition where
  lowerBound (IMRange Range{_start}) = IMPosition _start
  upperBound (IMRange Range{_end}) = IMPosition _end
  rightClosed _ = False

instance Pretty' SpanInfo where
  pretty' = \case
    SpanInfo'Name name -> pretty' name
    SpanInfo'ZnType ty -> pretty' ty

instance Pretty' IMPosition where
  pretty' (IMPosition Position{_line, _character}) =
    pretty' (_line + 1) <> ":" <> pretty' (_character + 1)

instance Pretty' IMRange where
  pretty' (IMRange Range{_start, _end}) =
    (pretty' (IMPosition _start))
      <> "-"
      <> (pretty' (IMPosition _end))
