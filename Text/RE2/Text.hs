module Text.RE2.Text
    ( RE2
    , compile
    , match
    , stats
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import Text.RE2.ByteString ( RE2, stats )
import qualified Text.RE2.ByteString as B

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

compile :: [Option] -> T.Text -> Either Error RE2
compile opts txt = mapM_ check opts >> go where
    check (Encoding _) = Left (Error Nothing msg BS.empty) where
        msg = "Text.RE2.Text.compile: cannot specify Encoding option"
    check _ = Right ()
    go = B.compile (Encoding UTF8 : opts) (T.encodeUtf8 txt)

match :: MatchOptions -> RE2 -> T.Text -> Maybe (Match T.Text)
match opts re txt = fmap (fmap T.decodeUtf8) . B.match opts re $ T.encodeUtf8 txt
