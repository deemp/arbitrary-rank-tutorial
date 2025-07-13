module Language.Arralac.Syntax.Local.Name where

import GHC.Generics (Generic)
import Language.Arralac.Prelude.Pretty (Pretty' (..), PrettyVerbosity (..))
import Language.Arralac.Prelude.Types (FastString)
import Language.Arralac.Prelude.Unique (Unique)
import Language.Arralac.Prelude.Unique.Supply (CtxUniqueSupply, newUnique)
import Prettyprinter

-- =======
-- [Names]
-- =======

-- | A unique, unambiguous name for something, containing information about where that thing originated.
--
-- Similar to @Name@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L126
data Name = Name
  { nameOcc :: OccName
  , nameUnique :: {-# UNPACK #-} !Unique
  , nameLoc :: !SrcSpan
  -- , nameSort :: NameSort
  -- See https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L148
  }
  deriving stock (Generic)

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

-- | Variables namespace.
--
-- Similar to @NameSpace@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L144
data NameSpace
  = -- | Term variables.
    NameSpace'TermVar
  | -- | Type variables.
    NameSpace'TyVar
  | -- | Built-in types.
    NameSpace'TyConcrete
  deriving stock (Eq, Show)

-- ===================
-- [Source code spans]
-- ===================

-- | Source Span
--
-- A 'SrcSpan' identifies either a specific portion of a text file
-- or a human-readable description of a location.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L392
data SrcSpan
  = RealSrcSpan RealSrcSpan
  | UnhelpfulSpan UnhelpfulSpanReason

-- | Real Source Span
--
-- It's open on the right: [start; finish)
-- It's zero-based here. Not sure about GHC.
--
-- Similar to @RealSrcSpan@ in GHC.
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

-- | Unknown span.
--
-- Similar to @UnhelpfulSpanReason@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L399
data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulGenerated
  | UnhelpfulOther !FastString
  deriving stock (Eq, Show)

-- ================
-- [Creating Names]
-- ================

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L465
noSrcSpan :: SrcSpan
noSrcSpan = UnhelpfulSpan UnhelpfulNoLocationInfo

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L548
mkSystemName :: Unique -> OccName -> Name
mkSystemName uniq occ = mkSystemNameAt uniq occ noSrcSpan

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L552
mkSystemNameAt :: Unique -> OccName -> SrcSpan -> Name
mkSystemNameAt uniq occ loc =
  Name
    { nameUnique = uniq
    , nameOcc = occ
    , nameLoc = loc
    }

-- | Create a new 'Name'.
--
-- Similar to @newSysName@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Monad.hs#L735
newSysName :: (CtxUniqueSupply) => OccName -> IO Name
newSysName occ =
  do
    uniq <- newUnique
    pure (mkSystemName uniq occ)

-- ==========================
-- [Instances for code spans]
-- ==========================

instance Pretty' RealSrcSpan where
  pretty' r =
    (pretty' r.srcSpanFile <> ":")
      <> (pretty' (r.srcSpanSLine + 1) <> ":" <> pretty' (r.srcSpanSCol + 1))
      <> "-"
      -- https://github.com/microsoft/vscode/blob/7b1c3d3dcee38406280f67f9909508680da0898d/src/vs/workbench/contrib/terminalContrib/links/browser/terminalLinkParsing.ts#L79
      <> (pretty' (r.srcSpanELine + 1) <> "." <> pretty' (r.srcSpanECol + 1))

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
          NameSpace'TyConcrete -> pretty' name.nameOcc.occNameFS
          _ -> pretty' name.nameOcc.occNameFS <> "_" <> pretty' name.nameUnique
      PrettyVerbosity'Detailed ->
        pretty' name.nameOcc.occNameFS
          <> brackets ("ID" <+> pretty' name.nameUnique <> "," <+> pretty' name.nameLoc)
      PrettyVerbosity'User ->
        pretty' name.nameOcc.occNameFS
