{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}

module System.Socket.Family.Unix 
    ( Unix
    , SocketAddress
    , socketAddressUnixPath
    , socketAddressUnixAbstract
    , getUnixPath
    ) where

import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable(..))
import           Foreign.Marshal.Utils (fillBytes, copyBytes)

import           Data.Word (Word16, Word8)
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString as B

import          System.Socket (Family(..), SocketAddress(..), Protocol(..))

#include "hs_socket.h"

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | The [Unix domain socket]
-- (https://en.wikipedia.org/wiki/Unix_domain_socket)
data Unix

instance Family Unix where
    familyNumber _ = (#const AF_UNIX)

-- TODO move to main repository
instance Protocol Unix where
    protocolNumber _ = 0

-- | A Unix socket address 
data instance SocketAddress Unix
    = SocketAddressUnixPath ByteString 
    | SocketAddressUnixAbstract ByteString
    deriving (Eq, Show)

-- | The maximal length of a path. 1 byte is reserved for null byte.
-- TODO docs about number
maxPathLength :: Int
maxPathLength = 107

-- TODO better names?
socketAddressUnixPath :: ByteString -> Maybe (SocketAddress Unix)
socketAddressUnixPath path 
    | B.length path <= maxPathLength = Just $ SocketAddressUnixPath path
    | otherwise = Nothing

socketAddressUnixAbstract :: ByteString -> Maybe (SocketAddress Unix)
socketAddressUnixAbstract path 
    | len <= maxPathLength = Just . SocketAddressUnixAbstract $
        path `B.append` B.replicate (maxPathLength - len) 0
    | otherwise = Nothing
  where len = B.length path

getUnixPath :: SocketAddress Unix -> Maybe (ByteString)
getUnixPath (SocketAddressUnixPath path) = Just path
getUnixPath _ = Nothing

instance Storable (SocketAddress Unix) where
    sizeOf    _ = (#size struct sockaddr_un)
    alignment _ = (#alignment struct sockaddr_un)

    peek ptr = do
        first <- peek (sun_path ptr) :: IO Word8
        case first of
            0 -> SocketAddressUnixAbstract <$> 
                    B.packCStringLen (castPtr $ sun_path ptr `plusPtr` 1, maxPathLength)
            _ -> SocketAddressUnixPath <$> B.packCString (castPtr $ sun_path ptr)
      where
        sun_path   = (#ptr struct sockaddr_un, sun_path)

    poke ptr socketAddress = do
        fillBytes ptr 0 (#const sizeof(struct sockaddr_un))
        poke (sun_family ptr) ((#const AF_UNIX) :: Word16)
        case socketAddress of
            SocketAddressUnixPath path -> unsafeUseAsCStringLen path $
                \(src, len) -> copyBytes (sun_path ptr) src len
            SocketAddressUnixAbstract path -> unsafeUseAsCStringLen path $
                \(src, len) -> copyBytes (sun_path ptr `plusPtr` 1) src len
      where
        sun_family = (#ptr struct sockaddr_un, sun_family)
        sun_path   = (#ptr struct sockaddr_un, sun_path)
