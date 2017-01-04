{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}

module System.Socket.Family.Unix 

where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils (fillBytes, copyBytes)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word16)

import System.Socket (Family(..), SocketAddress(..), Protocol(..))

#include "hs_socket.h"

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | The [Unix domain socket]
-- (https://en.wikipedia.org/wiki/Unix_domain_socket)
data Unix

instance Family Unix where
    familyNumber _ = (#const AF_UNIX)

instance Protocol Unix where
    protocolNumber _ = 0

-- | A Unix socket address
data instance SocketAddress Unix
    = SocketAddressUnix { unixPath :: ByteString }
    deriving (Eq, Show)

instance Storable (SocketAddress Unix) where
    sizeOf    _ = (#size struct sockaddr_un)
    alignment _ = (#alignment struct sockaddr_un)

    peek ptr = do
        undefined
    poke ptr socketAddress = do
        fillBytes ptr 0 (#const sizeof(struct sockaddr_un))
        poke (sun_family ptr) ((#const AF_UNIX) :: Word16)
        unsafeUseAsCStringLen (unixPath socketAddress) $ \(src, len) ->
            copyBytes (sun_path ptr) src len
      where
        sun_family = (#ptr struct sockaddr_un, sun_family)
        sun_path   = (#ptr struct sockaddr_un, sun_path)
