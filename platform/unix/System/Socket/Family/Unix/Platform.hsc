-- |
-- Stability   :  experimental
-- Portability :  Unix

{-# options_ghc -fno-warn-orphans #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}

module System.Socket.Family.Unix.Platform
    ( SocketAddress
    , socketAddressUnixPath
    , getUnixPath
    ) where

import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable(..))
import           Foreign.Marshal.Utils (fillBytes, copyBytes)

import           Data.Word (Word16 )
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString as B

import           System.Socket (SocketAddress)
import           System.Socket.Family.Unix.Internal (Unix)

#include "hs_socket.h"

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | A Unix socket address
data instance SocketAddress Unix
    -- | Address is connected to a filesystem pathname. When used to bind
    -- a socket file with this name is created in the file system.
    = SocketAddressUnixPath ByteString
    deriving (Eq, Show)

-- | The maximal length of a address path.
-- SUSv3 doesn’t specify the size of the sun_path field. Early BSD
-- implementations used 108 and 104 bytes, and one contemporary implementation
-- (HP-UX 11) uses 92 bytes.  On linux it is declared as
-- > char sun_path[108];
-- and 1 byte is reserved for null byte.
maxPathLength :: Int
maxPathLength = 107

-- | Creates address which is connected to a filesystem pathname.
-- Returns Nothing if @path@'s length exceeds maximal supported.
socketAddressUnixPath :: ByteString -> Maybe (SocketAddress Unix)
socketAddressUnixPath path
    | B.length path <= maxPathLength = Just $ SocketAddressUnixPath path
    | otherwise = Nothing

-- | Returns filesystem pathname where address is connected to.
getUnixPath :: SocketAddress Unix -> Maybe (ByteString)
getUnixPath (SocketAddressUnixPath path) = Just path

-- For implementation details see @man unix@
instance Storable (SocketAddress Unix) where
    sizeOf    _ = (#size struct sockaddr_un)
    alignment _ = (#alignment struct sockaddr_un)

    peek ptr = SocketAddressUnixPath <$> B.packCString (castPtr $ sun_path ptr)
      where
        sun_path   = (#ptr struct sockaddr_un, sun_path)

    poke ptr (SocketAddressUnixPath path) = do
        fillBytes ptr 0 (#const sizeof(struct sockaddr_un))
        poke (sun_family ptr) ((#const AF_UNIX) :: Word16)
        unsafeUseAsCStringLen path $
            \(src, len) -> copyBytes (sun_path ptr) src len
      where
        sun_family = (#ptr struct sockaddr_un, sun_family)
        sun_path   = (#ptr struct sockaddr_un, sun_path)
