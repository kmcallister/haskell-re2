{-# LANGUAGE
    DeriveDataTypeable #-}
module Text.RE2.Types
    ( Option(..)
    , Encoding(..)
    , Error(..)
    ) where

import qualified Data.ByteString as B

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
