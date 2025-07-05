{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.STLC.LanguageServer.IntervalMap where

import Data.IntervalMap.Generic.Strict qualified as IM
import Language.STLC.Typing.Jones2007.BasicTypes (AnnoZn (..), CompZn, Concrete (..), Name (..), RealSrcSpan (..), SrcSpan (..), SynTerm (..), SynType (..), ZnTermVar (..), ZnTyVar (..), ZnType)
import Prettyprinter (Pretty (..))

data SrcPosition = SrcPosition
  { srcPositionLine :: {-# UNPACK #-} !Int
  , srcPositionCol :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord)

data IM'RealSrcSpan
  = IM'RealSrcSpan
  { imSrcSpanSLine :: {-# UNPACK #-} !Int
  , imSrcSpanSCol :: {-# UNPACK #-} !Int
  , imSrcSpanELine :: {-# UNPACK #-} !Int
  , imSrcSpanECol :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

data SpanInfo
  = SpanInfo'Name Name
  | SpanInfo'SrcSpan SrcSpan
  | SpanInfo'ZnType ZnType

instance Pretty SpanInfo where
  pretty = \case
    SpanInfo'Name name -> pretty name
    SpanInfo'SrcSpan span' -> pretty span'
    SpanInfo'ZnType ty -> pretty ty

instance Show SpanInfo where
  show = show . pretty

goTermVar :: ZnTermVar -> [(SrcSpan, SpanInfo)]
goTermVar var = [(var.varName.nameLoc, SpanInfo'ZnType var.varType)]

goTyVar :: ZnTyVar -> [(SrcSpan, SpanInfo)]
goTyVar var = [(var.varName.nameLoc, SpanInfo'Name var.varName)]

goType :: SynType CompZn -> [(SrcSpan, SpanInfo)]
goType = \case
  SynType'Var _ var -> goTyVar var
  SynType'ForAll anno tvs body -> [(anno, SpanInfo'SrcSpan anno)] <> (concat $ goTyVar <$> tvs) <> goType body
  SynType'Fun anno arg res -> [(anno, SpanInfo'SrcSpan anno)] <> goType arg <> goType res
  SynType'Concrete _ ty -> [(ty.concreteName.nameLoc, SpanInfo'Name ty.concreteName)]

goTerm :: SynTerm CompZn -> [(SrcSpan, SpanInfo)]
goTerm = \case
  SynTerm'Var _ var -> goTermVar var
  SynTerm'Lit anno _ -> f anno
  SynTerm'App anno fun arg -> f anno <> goTerm fun <> goTerm arg
  SynTerm'Lam anno var body -> f anno <> goTermVar var <> goTerm body
  SynTerm'ALam anno var ty body -> f anno <> goTermVar var <> goType ty <> goTerm body
  SynTerm'Let anno var val body -> f anno <> goTermVar var <> goTerm val <> goTerm body
  SynTerm'Ann anno body ty -> f anno <> goTerm body <> goType ty
 where
  f AnnoZn{annoSrcLoc, annoType} = [(annoSrcLoc, SpanInfo'ZnType annoType)]

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
toIntervalMap :: SynTerm CompZn -> IM.IntervalMap IM'RealSrcSpan SpanInfo
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
          ( IM'RealSrcSpan
              { imSrcSpanSLine = srcSpanSLine
              , imSrcSpanSCol = srcSpanSCol
              , imSrcSpanELine = srcSpanELine
              , imSrcSpanECol = srcSpanECol
              }
          , info
          )
    (UnhelpfulSpan span', info) -> Left (span', info)
  spanInfo = snd (partitionWith isRealSrcSpan (goTerm term))

k :: SynTerm CompZn
k = undefined

t1 :: SrcPosition
t1 = undefined

toImRealSrcSpan :: SrcPosition -> IM'RealSrcSpan
toImRealSrcSpan
  SrcPosition
    { srcPositionCol
    , srcPositionLine
    } =
    IM'RealSrcSpan
      { imSrcSpanSLine = srcPositionLine
      , imSrcSpanSCol = srcPositionCol
      , imSrcSpanELine = srcPositionLine
      , imSrcSpanECol = srcPositionCol
      }

lookupAtSrcPosition :: SrcPosition -> IM.IntervalMap IM'RealSrcSpan a -> Maybe (IM'RealSrcSpan, a)
lookupAtSrcPosition point m =
  case res of
    Nothing -> Nothing
    Just (span', _) ->
      if IM.inside point span'
        then res
        else Nothing
 where
  res = IM.lookupLE (toImRealSrcSpan point) m

instance Ord IM'RealSrcSpan where
  x <= y =
    x.imSrcSpanSLine < y.imSrcSpanSLine
      || x.imSrcSpanSLine == y.imSrcSpanSLine
        && x.imSrcSpanSCol < y.imSrcSpanSCol
      || x.imSrcSpanSLine == y.imSrcSpanSLine
        && x.imSrcSpanSCol == y.imSrcSpanSCol
        && x.imSrcSpanELine > y.imSrcSpanELine
      || x.imSrcSpanSLine == y.imSrcSpanSLine
        && x.imSrcSpanSCol == y.imSrcSpanSCol
        && x.imSrcSpanELine == y.imSrcSpanELine
        && x.imSrcSpanECol >= y.imSrcSpanECol

instance IM.Interval IM'RealSrcSpan SrcPosition where
  lowerBound
    IM'RealSrcSpan
      { imSrcSpanSLine
      , imSrcSpanSCol
      } =
      SrcPosition
        { srcPositionLine = imSrcSpanSLine
        , srcPositionCol = imSrcSpanSCol
        }
  upperBound
    IM'RealSrcSpan
      { imSrcSpanELine
      , imSrcSpanECol
      } =
      SrcPosition
        { srcPositionLine = imSrcSpanELine
        , srcPositionCol = imSrcSpanECol
        }
  rightClosed _ = False

instance Pretty SrcPosition where
  pretty SrcPosition{srcPositionLine, srcPositionCol} =
    pretty srcPositionLine <> ":" <> pretty srcPositionCol

instance Show SrcPosition where
  show = show . pretty

instance Pretty IM'RealSrcSpan where
  pretty
    IM'RealSrcSpan
      { imSrcSpanSLine
      , imSrcSpanSCol
      , imSrcSpanELine
      , imSrcSpanECol
      } =
      (pretty imSrcSpanSLine <> ":" <> pretty imSrcSpanSCol)
        <> "-"
        <> (pretty imSrcSpanELine <> ":" <> pretty imSrcSpanECol)
