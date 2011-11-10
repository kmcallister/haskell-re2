-- | Regex matching for UTF8-encoded @'B.ByteString'@s.
module Text.RE2.ByteString.UTF8
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

-- | Compile a regex, given as a UTF8-encoded @'B.ByteString'@.
compile :: [CompileOption] -> B.ByteString -> Either Error (RE2 UTF8)
compile = P.compile P.UTF8

-- | Match a compiled regex against the given UTF8-encoded @'B.ByteString'@.
match :: MatchOptions -> RE2 UTF8 -> B.ByteString -> Maybe (Match B.ByteString)
match = P.match
