{-# LANGUAGE
    DeriveDataTypeable
  , EmptyDataDecls #-}
{-# OPTIONS_GHC
    -fno-warn-unused-imports #-}
-- | Types used in compiling and matching regexes.
module Text.RE2.Types
    ( -- * Regular expressions
      RE2
    , UTF8, Latin1

      -- * Compile options
    , CompileOption(..)

      -- * Match options
    , MatchOptions(..)
    , defMatchOptions
    , Anchor(..)

      -- * Results
    , Error(..)
    , Match(..)
    , Group(..)
    , Stats(..)
    ) where

import Text.RE2.Types.Internal ( RE2, UTF8, Latin1 )

import qualified Data.ByteString as B
import qualified Data.Sequence   as S

import Data.Default
import Data.Text       ( Text       )
import Data.ByteString ( ByteString )
import Data.Data       ( Typeable, Data )


-- | Options available when compiling a regex.
--
-- Defaults (in parentheses) are according to
-- <http://code.google.com/p/re2/source/browse/re2/re2.h>. The Haskell binding
-- does not enforce these.
--
-- If multiple options with the same constructor are given in a list,
-- the last one has priority.
data CompileOption
    = PosixSyntax   Bool  -- ^ (False) Restrict regexps to POSIX egrep syntax.
    | LongestMatch  Bool  -- ^ (False) Search for longest match, not first match.
    | LogErrors     Bool  -- ^ (True)  Print errors to stderr.
    | Literal       Bool  -- ^ (False) Interpret string as literal, not regexp.
    | NeverNewline  Bool  -- ^ (False) Never match @\\n@, even if it is in regexp.
    | CaseSensitive Bool  -- ^ (True)  Match is case-sensitive.
    | PerlClasses   Bool  -- ^ Allow Perl's @\\d \\s \\w \\D \\S \\W@ even in POSIX mode.
    | WordBoundary  Bool  -- ^ Allow Perl's @\\b \\B@ even in POSIX mode.
    | OneLine       Bool  -- ^ Match @^@ and @$@ only at beginning and end of text even in POSIX mode.
    | MaxMemory     Int   -- ^ Memory limit, in bytes.
    deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | An error which occurred while compiling a regex.
data Error = Error
    { -- | Errors which originate in RE2 itself have a numeric code.  For now,
      -- the meaning of these codes is not exposed in Haskell.
      errCode     :: Maybe Int

      -- | Human-readable description of the error.
    , errMessage  :: String

      -- | A fragment of the regex relating to the error.  When the regex was
      -- given as @'String'@ or @'Text'@, this fragment will be UTF-8.
    , errFragment :: B.ByteString
    } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | Is a match required to coincide with the start or end of input?
data Anchor
    = Unanchored   -- ^ No anchor; match may occur anywhere inside input.
    | AnchorStart  -- ^ Anchor at start only.
    | AnchorBoth   -- ^ Anchor at start and end.
    deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

-- | Options available when matching with a regex.
--
-- For default options use @'defMatchOptions'@, or import @Data.Default@
-- and use @'def'@.
data MatchOptions = MatchOptions
    { -- | Anchor match at start or end of input?
      anchor :: Anchor

      -- | If @'Just' n@, only store the first @n@ capturing groups.  This
      -- includes the whole-match group, so @Just 1@ returns only that group,
      -- and @Just 0@ is a Boolean match only.
    , captureLimit :: Maybe Int
    } deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | Default @'MatchOptions'@.
defMatchOptions :: MatchOptions
defMatchOptions = MatchOptions
    { anchor       = Unanchored
    , captureLimit = Nothing }

instance Default MatchOptions where
    def = defMatchOptions

-- | A capturing group.
--
-- Parametrized by the string type: @'ByteString'@, @'Text'@, or @'String'@.
data Group str = Group
    { byteStart  :: Int  -- ^ The byte index of the start of this group.
    , byteLength :: Int  -- ^ The length of this group in bytes.
    , captured   :: str  -- ^ The string which was captured.
    } deriving (Eq, Ord, Show, Read, Typeable, Data)

instance Functor Group where
    fmap f gr = gr { captured = f (captured gr) }

-- | A successful regex match.
--
-- Groups which did not participate in this match are represented as
-- @'Nothing'@.
newtype Match str = Match (S.Seq (Maybe (Group str)))
    deriving (Eq, Ord, Show, Read, Typeable, Data)

instance Functor Match where
    fmap f (Match xs) = Match (fmap (fmap (fmap f)) xs)

-- | Some statistics about a compiled regex.
data Stats = Stats
    { -- | The number of capturing groups, not including the whole-match group.
      numGroups   :: Int
      -- | The program size, a rough cost measure.
    , programSize :: Int
    } deriving (Eq, Ord, Show, Read, Typeable, Data)
