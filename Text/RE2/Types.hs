{-# LANGUAGE
    DeriveDataTypeable #-}
module Text.RE2.Types
    ( CompileOption(..)
    , Encoding(..)
    , Error(..)
    , Match(..)
    , Stats(..)
    , MatchOptions(..)
    , defMatchOptions
    , Anchor(..)
    , Group(..)
    ) where

import qualified Data.ByteString as B
import qualified Data.Sequence   as S

import Data.Data ( Typeable, Data )

data Encoding
    = UTF8
    | Latin1
    deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

data CompileOption
    = PosixSyntax   Bool
    | LongestMatch  Bool
    | LogErrors     Bool
    | Literal       Bool
    | NeverNewline  Bool
    | CaseSensitive Bool
    | PerlClasses   Bool
    | WordBoundary  Bool
    | OneLine       Bool
    | MaxMemory     Int
    | Encoding      Encoding
    deriving (Eq, Ord, Show, Read, Typeable, Data)

data Error = Error
    { errCode     :: Maybe Int
    , errMessage  :: String
    , errFragment :: B.ByteString
    } deriving (Eq, Ord, Show, Read, Typeable, Data)

data Anchor
    = Unanchored
    | AnchorStart
    | AnchorBoth
    deriving (Eq, Ord, Show, Read, Typeable, Data, Enum, Bounded)

data MatchOptions = MatchOptions
    { moAnchor    :: Anchor
    , moNumGroups :: Maybe Int
    } deriving (Eq, Ord, Show, Read, Typeable, Data)

defMatchOptions :: MatchOptions
defMatchOptions = MatchOptions
    { moAnchor    = Unanchored
    , moNumGroups = Nothing }

data Group str = Group
    { grByteStart  :: Int
    , grByteLength :: Int
    , grString     :: str
    } deriving (Eq, Ord, Show, Read, Typeable, Data)

instance Functor Group where
    fmap f gr = gr { grString = f (grString gr) }

newtype Match str = Match (S.Seq (Maybe (Group str)))
    deriving (Eq, Ord, Show, Read, Typeable, Data)

instance Functor Match where
    fmap f (Match xs) = Match (fmap (fmap (fmap f)) xs)

data Stats = Stats
    { stNumCapturingGroups :: Int
    , stProgramSize        :: Int
    } deriving (Eq, Ord, Show, Read, Typeable, Data)
