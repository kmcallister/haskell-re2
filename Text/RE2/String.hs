-- | Regex matching for @'String'@s.
module Text.RE2.String
    ( -- * Building regexes
      compile

      -- * Using regexes
    , match
    , stats

      -- * Exported for convenience
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import Text.RE2.ByteString ( stats )
import qualified Text.RE2.ByteString as B

import qualified Data.ByteString.UTF8 as U

-- | Compile a regex, given as a @'String'@.
compile :: [CompileOption] -> String -> Either Error (RE2 UTF8)
compile opts = B.compileUTF8 opts . U.fromString

-- | Match a compiled regex against the given @'String'@.
match :: MatchOptions -> RE2 UTF8 -> String -> Maybe (Match String)
match opts re = fmap (fmap U.toString) . B.match opts re . U.fromString
