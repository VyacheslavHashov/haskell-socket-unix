-- |
-- Stability   :  experimental
-- Portability :  Linux, OS X

module System.Socket.Family.Unix
    ( Unix
    , module Exports
    -- * Exceptions
    , eNoEntry
    ) where

import System.Socket.Family.Unix.Internal
import System.Socket.Family.Unix.Platform as Exports

