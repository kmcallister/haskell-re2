module Text.RE2.Pure
    ( compile
    , match
    , stats
    , I.Encoding(..)
    ) where

import Text.RE2.Types
import qualified Text.RE2.IO as I

import qualified Data.ByteString as B

import System.IO.Unsafe ( unsafePerformIO )

-- unsafePerformIO is safe to use because:
--   * 'compile' creates a new object at a known state
--   * 'match' and 'stats' invoke const C++ methods
--   * re2.h asserts that multi-threaded use is OK

-- Does not tag the encoding properly.  This is the responsibility of the exposed
-- modules.
compile :: I.Encoding -> [CompileOption] -> B.ByteString -> Either Error (RE2 enc)
compile enc opts bs = unsafePerformIO (I.compile enc opts bs)

match :: MatchOptions -> RE2 enc -> B.ByteString -> Maybe (Match B.ByteString)
match opts re bs = unsafePerformIO (I.match opts re bs)

-- | Get some statistics about a regex.
stats :: RE2 enc -> Stats
stats re = unsafePerformIO (I.stats re)
