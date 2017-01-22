{-# language OverloadedStrings #-}
module Main where

import Control.Concurrent.Async (async, wait)
import Control.Exception (bracket, throwIO, try)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.ByteString (ByteString)
import System.Posix.Files (removeLink, fileExist)
import Test.Tasty
import Test.Tasty.HUnit

import System.Socket
import System.Socket.Type.Stream
import System.Socket.Type.Datagram
import System.Socket.Family.Unix

import Internal
import Platform

main :: IO ()
main = defaultMain $ testGroup "unix domain socket"
    [ groupUnixPathname
    , groupAbstractName ]

