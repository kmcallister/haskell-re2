{-# LANGUAGE
    DeriveDataTypeable #-}
module Text.RE2.IO
    ( RE2
    , compile
    , match
    , stats
    ) where

import Text.RE2.C
import Text.RE2.Types

import Foreign
import Foreign.C
import Control.Exception
import Control.Monad

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Sequence          as S

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

stats :: RE2 -> IO Stats
stats (RE2 p) = do
    ncg <- cre2_num_capturing_groups p
    pgs <- cre2_program_size p
    return (Stats (fromIntegral ncg) (fromIntegral pgs))

getAnch :: Anchor -> Anchor_t
getAnch Unanchored  = cre2Unanchored
getAnch AnchorStart = cre2AnchorStart
getAnch AnchorBoth  = cre2AnchorBoth

match :: MatchOptions -> RE2 -> B.ByteString -> IO (Result B.ByteString)
match mo (RE2 rep) bs = do
    nmatches <- case moNumGroups mo of
        Just n  -> return n
        Nothing -> ((+1) . fromIntegral) `fmap` cre2_num_capturing_groups rep
    allocaArray nmatches $ \arr ->
      B.unsafeUseAsCStringLen bs $ \(buf, len) -> do
        let clen = fromIntegral len
            cnmatches = fromIntegral nmatches
            anch = getAnch (moAnchor mo)
        ret <- cre2_match rep buf clen 0 clen anch arr cnmatches
        case ret of
            0 -> return NoMatch
            _ -> do
                pieces  <- peekArray nmatches arr
                matches <- mapM (getMatch bs buf) pieces
                return (Match (S.fromList matches))

getMatch
    :: B.ByteString
    -> Ptr CChar
    -> StringPiece
    -> IO (Maybe (Group B.ByteString))
getMatch inpt inpt_ptr (StringPiece ptr clen)
    | ptr == nullPtr  = return Nothing
    | otherwise = do
        let off = ptr `minusPtr` inpt_ptr
        when (off < 0) $
            throwIO (ErrorCall "Text.RE2: negative-offset matching group")
        let len = fromIntegral clen
            bs  = B.take len (B.drop off inpt)
        return (Just $ Group off len bs)
