-- | Regex matching for @'B.ByteString'@s.
module Text.RE2.ByteString
    ( -- * Regex matching
      compile
    , match

      -- * Types
    , RE2
    , stats
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import Text.RE2.IO ( RE2 )
import qualified Text.RE2.IO as I

import qualified Data.ByteString as B

import System.IO.Unsafe ( unsafePerformIO )

-- unsafePerformIO is safe to use because:
--   * 'compile' creates a new object at a known state
--   * 'match' and 'stats' invoke const C++ methods
--   * re2.h asserts that multi-threaded use is OK

-- | Compile a regex, given as a @'B.ByteString'@.
--
-- The specified character encoding is used to interpret the regex and any
-- subsequent input to @'match'@.
compile :: Encoding -> [CompileOption] -> B.ByteString -> Either Error RE2
compile enc opts bs = unsafePerformIO (I.compile enc opts bs)

-- | Match a compiled regex against the given @'B.ByteString'@.
--
-- The input is interpreted according to the character encoding
-- which was passed to @'compile'@.
match :: MatchOptions -> RE2 -> B.ByteString -> Maybe (Match B.ByteString)
match opts re bs = unsafePerformIO (I.match opts re bs)

-- | Get some statistics about a regex.
stats :: RE2 -> Stats
stats re = unsafePerformIO (I.stats re)
