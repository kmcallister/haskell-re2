{-# LANGUAGE
    DeriveDataTypeable
  , EmptyDataDecls #-}
module Text.RE2.Types.Internal
    ( RE2(..)
    , UTF8, Latin1
    ) where

import Text.RE2.C ( CRE2 )

import Foreign.ForeignPtr ( ForeignPtr )
import Data.Data ( Typeable )

-- | Abstract type representing a compiled regex.
--
-- Parametrized by a character encoding, either @'UTF8'@
-- or @'Latin1'@.
newtype RE2 enc = RE2 (ForeignPtr CRE2)
    deriving (Typeable)

-- | Tag type denoting a regex which operates on UTF8-encoded text.
data UTF8
    deriving (Typeable)

-- | Tag type denoting a regex which operates on Latin1-encoded text.
data Latin1
    deriving (Typeable)
