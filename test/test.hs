{-# language OverloadedStrings #-}
module Main where

import Control.Exception (bracket, catch, throwIO, try)
import           Control.Concurrent.Async (async, wait)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Test.Tasty
import Test.Tasty.HUnit
import System.Posix.Files (removeLink)

import System.Socket
import System.Socket.Type.Stream
import System.Socket.Type.Datagram
import System.Socket.Family.Unix


main :: IO ()
main = defaultMain $ testGroup "unix domain socket"
    [ groupUnixPathname
    , groupAbstractName
    ]

unixPath :: IsString a => a
unixPath = "Woum5ag3oohuaLee.socket"

abstractPath :: ByteString
abstractPath = "/tmp/uth4Aechiereejae.socket"

unixSocket :: IO (Socket Unix Stream Unix)
unixSocket = socket

groupUnixPathname :: TestTree
groupUnixPathname = testGroup "Unix path name"
    [ testCase "connect to non-existing path name" $ bracket
        unixSocket
        close
        (\s -> do
            r <- try $ connect s (fromJust $ socketAddressUnixPath unixPath)
            case r of
                Left e | e == SocketException 2 -> return ()
                       | otherwise              -> throwIO e
                Right () -> assertFailure "connection should have failed"
        )
    , testCase "server\\client" $ bracket
        ( (,) <$> unixSocket <*> unixSocket)
        (\(server, client) -> do
            close server
            close client
            removeLink unixPath
        )
        (testServerClient . fromJust $ socketAddressUnixPath unixPath)
    ]

testServerClient
    :: SocketAddress Unix
    -> (Socket Unix Stream Unix, Socket Unix Stream Unix)
    -> IO ()
testServerClient addr (server, client) = do
    let messageSent = "Hello, World!"
    bind server addr
    listen server 5
    serverRecv <- async $ do
        (peerSock, peerAddr) <- accept server
        receive peerSock 4096 mempty
    connect client addr
    send client messageSent mempty
    messageReceived <- wait serverRecv
    when (messageReceived /= messageSent) $
        assertFailure "Received message is invalid"

groupAbstractName :: TestTree
groupAbstractName = testGroup "Abstract path name"
    [ testCase "connect to non-existing path name" $ bracket
        unixSocket
        close
        (\s -> do
            r <- try $ connect s (fromJust $ socketAddressUnixAbstract abstractPath)
            case r of
                Left e | e == eConnectionRefused -> return ()
                       | otherwise               -> throwIO e
                Right () -> assertFailure "connection should have failed"
        )
    , testCase "server\\client" $ bracket
        ( (,) <$> unixSocket <*> unixSocket)
        (\(server, client) -> do
            close server
            close client
        )
        (testServerClient . fromJust $ socketAddressUnixAbstract abstractPath)
    ]

