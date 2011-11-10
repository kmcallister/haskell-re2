-- | Regex matching for Latin1-encoded @'B.ByteString'@s.
module Text.RE2.ByteString.Latin1
    ( -- * Building regexes
      compile

      -- * Using regexes
    , match
    , P.stats

      -- * Exported for convenience
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import qualified Text.RE2.Pure as P

import qualified Data.ByteString as B

-- | Compile a regex, given as a Latin1-encoded @'B.ByteString'@.
compile :: [CompileOption] -> B.ByteString -> Either Error (RE2 Latin1)
compile = P.compile P.Latin1

-- | Match a compiled regex against the given Latin1-encoded @'B.ByteString'@.
match :: MatchOptions -> RE2 Latin1 -> B.ByteString -> Maybe (Match B.ByteString)
match = P.match
