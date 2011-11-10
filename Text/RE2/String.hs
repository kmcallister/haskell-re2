-- | Regex matching for @'String'@s.
module Text.RE2.String
    ( -- * Building regexes
      compile

      -- * Using regexes
    , match
    , B.stats

      -- * Exported for convenience
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import qualified Text.RE2.ByteString.UTF8 as B

import qualified Data.ByteString.UTF8 as U

-- | Compile a regex, given as a @'String'@.
compile :: [CompileOption] -> String -> Either Error (RE2 UTF8)
compile opts = B.compile opts . U.fromString

-- | Match a compiled regex against the given @'String'@.
match :: MatchOptions -> RE2 UTF8 -> String -> Maybe (Match String)
match opts re = fmap (fmap U.toString) . B.match opts re . U.fromString
