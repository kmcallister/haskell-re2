module Text.RE2.ByteString
    ( RE2
    , compile
    , match
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

compile :: [Option] -> B.ByteString -> Either Error RE2
compile opts bs = unsafePerformIO (I.compile opts bs)

match :: MatchOptions -> RE2 -> B.ByteString -> Maybe (Match B.ByteString)
match opts re bs = unsafePerformIO (I.match opts re bs)

stats :: RE2 -> Stats
stats re = unsafePerformIO (I.stats re)
