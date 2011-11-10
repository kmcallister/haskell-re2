-- | Regex matching for @'T.Text'@.
module Text.RE2.Text
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

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

-- | Compile a regex, given as a @'T.Text'@.
compile :: [CompileOption] -> T.Text -> Either Error (RE2 UTF8)
compile opts = B.compileUTF8 opts . T.encodeUtf8

-- | Match a compiled regex against the given @'T.Text'@.
match :: MatchOptions -> RE2 UTF8 -> T.Text -> Maybe (Match T.Text)
match opts re = fmap (fmap T.decodeUtf8) . B.match opts re . T.encodeUtf8
