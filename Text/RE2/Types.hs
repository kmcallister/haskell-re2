{-# LANGUAGE
    DeriveDataTypeable #-}
module Text.RE2.Types
    ( Option(..)
    , Encoding(..)
    , Error(..)
    , Result(..)
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

data Option
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
    { errCode     :: Int
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

data Result str =
      NoMatch
    | Match (S.Seq (Maybe (Group str)))
    deriving (Eq, Ord, Show, Read, Typeable, Data)
