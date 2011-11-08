module Text.RE2.String
    ( RE2
    , compile
    , match
    , stats
    , module Text.RE2.Types
    ) where

import Text.RE2.Types
import Text.RE2.Internal   ( forceUTF8  )
import Text.RE2.ByteString ( RE2, stats )
import qualified Text.RE2.ByteString as B

import qualified Data.ByteString.UTF8 as U

compile :: [Option] -> String -> Either Error RE2
compile opts str = do
    newopts <- forceUTF8 "Text.RE2.String.compile" opts
    B.compile newopts (U.fromString str)

match :: MatchOptions -> RE2 -> String -> Maybe (Match String)
match opts re txt = fmap (fmap U.toString) . B.match opts re $ U.fromString txt
