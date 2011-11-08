module Text.RE2.Internal
    ( forceUTF8
    ) where

import Text.RE2.Types

import qualified Data.ByteString as BS

forceUTF8 :: String -> [CompileOption] -> Either Error [CompileOption]
forceUTF8 fun_name opts = mapM_ check opts >> return (Encoding UTF8 : opts) where
    check (Encoding _) = Left (Error Nothing msg BS.empty) where
        msg = fun_name ++ ": cannot specify Encoding option"
    check _ = Right ()
