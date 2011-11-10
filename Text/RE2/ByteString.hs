-- | Regex matching for @'B.ByteString'@s.
module Text.RE2.ByteString
    ( -- * Building regexes
      compileUTF8
    , compileLatin1

      -- * Using regexes
    , match
    , stats

      -- * Exported for convenience
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import qualified Text.RE2.IO as I

import qualified Data.ByteString as B

import System.IO.Unsafe ( unsafePerformIO )

-- unsafePerformIO is safe to use because:
--   * 'compile' creates a new object at a known state
--   * 'match' and 'stats' invoke const C++ methods
--   * re2.h asserts that multi-threaded use is OK

-- | Compile a regex, given as a UTF8-encoded @'B.ByteString'@.
compileUTF8 :: [CompileOption] -> B.ByteString -> Either Error (RE2 UTF8)
compileUTF8 opts bs = unsafePerformIO (I.compile I.UTF8 opts bs)

-- | Compile a regex, given as a Latin1-encoded @'B.ByteString'@.
compileLatin1 :: [CompileOption] -> B.ByteString -> Either Error (RE2 Latin1)
compileLatin1 opts bs = unsafePerformIO (I.compile I.Latin1 opts bs)

-- | Match a compiled regex against the given @'B.ByteString'@.
--
-- The character encoding of the input should match the type argument to
-- @'RE2'@.
match :: MatchOptions -> RE2 enc -> B.ByteString -> Maybe (Match B.ByteString)
match opts re bs = unsafePerformIO (I.match opts re bs)

-- | Get some statistics about a regex.
stats :: RE2 enc -> Stats
stats re = unsafePerformIO (I.stats re)
