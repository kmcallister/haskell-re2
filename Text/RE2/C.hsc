{-# LANGUAGE
    ForeignFunctionInterface
  , EmptyDataDecls #-}
module Text.RE2.C where

import Foreign
import Foreign.C
import Control.Monad

#include <cre2.h>


data CRE2_Options

foreign import ccall cre2_opt_new
    :: IO (Ptr CRE2_Options)
foreign import ccall cre2_opt_delete
    :: Ptr CRE2_Options -> IO ()

foreign import ccall cre2_opt_posix_syntax
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_longest_match
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_log_errors
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_literal
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_never_nl
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_case_sensitive
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_perl_classes
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_word_boundary
    :: Ptr CRE2_Options -> CInt -> IO ()
foreign import ccall cre2_opt_one_line
    :: Ptr CRE2_Options -> CInt -> IO ()

foreign import ccall cre2_opt_max_mem
    :: Ptr CRE2_Options -> CInt -> IO ()

type Encoding_t = CInt
#enum Encoding_t,, CRE2_UTF8, CRE2_Latin1
foreign import ccall cre2_opt_encoding
    :: Ptr CRE2_Options -> Encoding_t -> IO ()


data StringPiece

sizeof_StringPiece :: Int
sizeof_StringPiece = (#size struct string_piece)

peek_StringPiece :: Ptr StringPiece -> IO CStringLen
peek_StringPiece ptr = liftM2 (,)
    ((#peek struct string_piece, data)   ptr)
    ((#peek struct string_piece, length) ptr)


data CRE2

foreign import ccall cre2_new
    :: CString -> Ptr CRE2_Options -> IO (Ptr CRE2)
foreign import ccall cre2_delete
    :: Ptr CRE2 -> IO ()

foreign import ccall cre2_error_code
    :: Ptr CRE2 -> IO CInt
foreign import ccall cre2_error_string
    :: Ptr CRE2 -> IO CString
foreign import ccall cre2_error_arg
    :: Ptr CRE2 -> Ptr StringPiece -> IO ()
foreign import ccall cre2_num_capturing_groups
    :: Ptr CRE2 -> IO CInt
foreign import ccall cre2_program_size
    :: Ptr CRE2 -> IO CInt

type Anchor_t = CInt
#enum Anchor_t,, CRE2_UNANCHORED, CRE2_ANCHOR_START, CRE2_ANCHOR_BOTH


foreign import ccall cre2_match
    :: Ptr CRE2
    -> CString
    -> CInt
    -> CInt
    -> Anchor_t
    -> Ptr StringPiece
    -> CInt
    -> IO Int

-- vim: ft=haskell
