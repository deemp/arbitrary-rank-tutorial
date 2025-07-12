module Language.Arralac.Syntax.Local.Name where

import GHC.Generics (Generic)
import Language.Arralac.Utils.Pretty (Pretty' (..), PrettyVerbosity (..))
import Language.Arralac.Utils.Types (FastString)
import Language.Arralac.Utils.Unique (Unique)
import Prettyprinter

-- import Language.Arralac.Syntax.Local.Type

-- ==============================================
-- [Names]
-- ==============================================

-- | A unique, unambiguous name for something, containing information about where that thing originated.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L126
data Name = Name
  { nameOcc :: OccName
  , nameUnique :: {-# UNPACK #-} !Unique
  , nameLoc :: !SrcSpan
  -- , nameSort :: NameSort
  -- See https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L148
  }
  deriving stock (Generic)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L144
data NameSpace
  = -- | Terms namespace.
    NameSpace'TermVar
  | -- | Types namespace.
    NameSpace'TypeVar
  | -- | Built-in types namespace.
    NameSpace'TypeConcrete
  deriving stock (Eq, Show)

-- | Occurrence Name.
--
-- In this context that means:
--
-- "classified (i.e. as a type name, value name, etc)
-- but not qualified
-- and not yet resolved".
--
-- Similar to @OccName@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L360
data OccName = OccName
  { occNameSpace :: !NameSpace
  , occNameFS :: !FastString
  }
  deriving stock (Show, Generic)

-- ==============================================
-- [Positions in the source code]
-- ==============================================

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L399
data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulGenerated
  | UnhelpfulOther !FastString
  deriving stock (Eq, Show)

-- TODO How does BNFC represent empty span?
-- Is its bound open on the right? Example : [start; finish)

-- | Real Source Span
--
-- It's open on the right: [start; finish)
--
-- It's zero-based here. Not sure about GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L367
data RealSrcSpan
  = RealSrcSpan'
  { srcSpanFile :: !FastString
  , srcSpanSLine :: {-# UNPACK #-} !Int
  , srcSpanSCol :: {-# UNPACK #-} !Int
  , srcSpanELine :: {-# UNPACK #-} !Int
  , srcSpanECol :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq)

-- | Source Span
--
-- A 'SrcSpan' identifies either a specific portion of a text file
-- or a human-readable description of a location.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L392
data SrcSpan
  = -- TODO Replace with RealSrcSpan
    RealSrcSpan RealSrcSpan
  | UnhelpfulSpan UnhelpfulSpanReason

-- ==============================================
-- [Miscellaneous instances]
-- ==============================================

instance Pretty' RealSrcSpan where
  pretty' r =
    (pretty' r.srcSpanFile <> ":")
      <> (pretty' (r.srcSpanSLine + 1) <> ":" <> pretty' (r.srcSpanSCol + 1))
      <> "-"
      <> (pretty' (r.srcSpanELine + 1) <> ":" <> pretty' (r.srcSpanECol + 1))

instance Pretty' UnhelpfulSpanReason where
  pretty' = \case
    UnhelpfulNoLocationInfo -> "No location"
    UnhelpfulGenerated -> "Generated"
    UnhelpfulOther reason -> pretty' reason

instance Pretty' SrcSpan where
  pretty' = \case
    RealSrcSpan sp -> pretty' sp
    UnhelpfulSpan reason -> "Unknown span:" <+> pretty' reason

-- ==============================================
-- [Instances for 'Name']
-- ==============================================

instance Eq Name where
  n1 == n2 = n1.nameUnique == n2.nameUnique

instance Ord Name where
  n1 <= n2 = n1.nameUnique <= n2.nameUnique

instance Pretty' Name where
  pretty' name =
    case ?prettyVerbosity of
      PrettyVerbosity'Normal ->
        case name.nameOcc.occNameSpace of
          NameSpace'TypeConcrete -> pretty' name.nameOcc.occNameFS
          _ -> pretty' name.nameOcc.occNameFS <> "_" <> pretty' name.nameUnique
      PrettyVerbosity'Detailed ->
        pretty' name.nameOcc.occNameFS
          <> brackets ("ID" <+> pretty' name.nameUnique <> "," <+> pretty' name.nameLoc)
      PrettyVerbosity'User ->
        pretty' name.nameOcc.occNameFS
