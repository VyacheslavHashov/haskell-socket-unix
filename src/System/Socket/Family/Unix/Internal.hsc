-- |
-- Stability   :  experimental
-- Portability :  Linux, Unix

module System.Socket.Family.Unix.Internal
    ( Unix
    -- * Exceptions
    , eNoEntry
    ) where

import System.Socket (SocketException(..))

#include "hs_socket.h"

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | The [Unix domain socket]
-- (https://en.wikipedia.org/wiki/Unix_domain_socket)
data Unix


-- | > SocketException "No such file or directory"
eNoEntry :: SocketException
eNoEntry = SocketException (#const ENOENT)

