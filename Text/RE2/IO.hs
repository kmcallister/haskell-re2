{-# LANGUAGE
    DeriveDataTypeable #-}
module Text.RE2.IO
    ( RE2
    , compile
    , numCapturingGroups
    , programSize
    , module Text.RE2.Types
    ) where

import Text.RE2.C
import Text.RE2.Types

import Foreign
import Foreign.C

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as B

import Data.Data ( Typeable )


setOption :: Ptr CRE2_Options -> Option -> IO ()
setOption opt = go where
    go (PosixSyntax   b) = goBool b cre2_opt_posix_syntax
    go (LongestMatch  b) = goBool b cre2_opt_longest_match
    go (LogErrors     b) = goBool b cre2_opt_log_errors
    go (Literal       b) = goBool b cre2_opt_literal
    go (NeverNewline  b) = goBool b cre2_opt_never_nl
    go (CaseSensitive b) = goBool b cre2_opt_case_sensitive
    go (PerlClasses   b) = goBool b cre2_opt_perl_classes
    go (WordBoundary  b) = goBool b cre2_opt_word_boundary
    go (OneLine       b) = goBool b cre2_opt_one_line
    go (MaxMemory     n) = cre2_opt_max_mem  opt (fromIntegral n)
    go (Encoding   UTF8) = cre2_opt_encoding opt cre2Utf8
    go (Encoding Latin1) = cre2_opt_encoding opt cre2Latin1

    goBool True  f = f opt 1
    goBool False f = f opt 0


newtype RE2 = RE2 (Ptr CRE2)
    deriving (Typeable)

nullTerminate :: B.ByteString -> B.ByteString
nullTerminate bs
    | B.null bs       = B.pack [0]
    | B.last bs == 0  = bs
    | otherwise       = B.snoc bs 0

getError :: CInt -> Ptr CRE2 -> IO Error
getError ec re = alloca $ \sp -> do
    msg <- cre2_error_string re >>= peekCString
    cre2_error_arg re sp
    StringPiece argdat arglen <- peek sp
    arg <- B.packCStringLen (argdat, fromIntegral arglen)
    return (Error (fromIntegral ec) msg arg)

compile :: [Option] -> B.ByteString -> IO (Either Error RE2)
compile opts pattern = do
    copts <- cre2_opt_new
    mapM_ (setOption copts) opts
    re <- B.unsafeUseAsCString (nullTerminate pattern) $
        flip cre2_new copts
    cre2_opt_delete copts

    ec <- cre2_error_code re
    if ec == 0
        then return (Right $ RE2 re)
        else Left `fmap` getError ec re

-- re2 copies pattern and options, so we do not need to hold onto
-- these structures.
--
-- FIXME: document null termination behavior
--
-- FIXME: finalizer

numCapturingGroups :: RE2 -> IO Int
numCapturingGroups (RE2 p) = fromIntegral `fmap` cre2_num_capturing_groups p

programSize :: RE2 -> IO Int
programSize (RE2 p) = fromIntegral `fmap` cre2_program_size p
