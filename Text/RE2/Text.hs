-- | Regex matching for @'T.Text'@.
module Text.RE2.Text
    ( -- * Regex matching
      compile
    , match

      -- * Types
    , RE2
    , stats
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import Text.RE2.ByteString ( RE2, stats )
import qualified Text.RE2.ByteString as B

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

-- | Compile a regex, given as a @'T.Text'@.
compile :: [CompileOption] -> T.Text -> Either Error (RE2 UTF8)
compile opts txt = B.compileUTF8 opts (T.encodeUtf8 txt)

-- | Match a compiled regex against the given @'T.Text'@.
match :: MatchOptions -> RE2 UTF8 -> T.Text -> Maybe (Match T.Text)
match opts re txt = fmap (fmap T.decodeUtf8) . B.match opts re $ T.encodeUtf8 txt
