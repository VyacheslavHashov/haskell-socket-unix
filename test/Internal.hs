{-# language OverloadedStrings #-}
module Internal where

import Control.Concurrent.Async (async, wait)
import Control.Exception (bracket, throwIO, try)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.ByteString (ByteString)
import System.Posix.Files.ByteString (removeLink, fileExist)
import Test.Tasty
import Test.Tasty.HUnit

import System.Socket
import System.Socket.Type.Stream
import System.Socket.Type.Datagram
import System.Socket.Family.Unix

groupUnixPathname :: TestTree
groupUnixPathname = testGroup "Unix path name"
    [ testCase "connect to non-existing path name" $ bracket
        unixSocketStream
        close
        (\s -> do
            r <- try $ connect s addr
            case r of
                Left e | e == eNoEntry -> return ()
                       | otherwise     -> throwIO e
                Right () -> assertFailure "connection should have failed"
        )

    , testCase "server\\client stream" $ bracket
        ( (,) <$> unixSocketStream <*> unixSocketStream)
        closeSockets
        (testServerClientStream addr)

    , testCase "server\\client datagram" $ bracket
        ((,) <$> unixSocketDatagram <*> unixSocketDatagram)
        closeSockets
        (testServerClientDatagram addr cAddr)
    ]
  where
    addr = fromJust $ socketAddressUnixPath unixPath
    cAddr = fromJust $ socketAddressUnixPath clientUnixPath
    closeSockets (server, client) = do
        close server
        close client
        unlink unixPath
        unlink clientUnixPath
    -- Sockets with real pathname should be unlinked after closing
    unlink path = fileExist path >>= flip when (removeLink path)

clientMessage :: ByteString
clientMessage = "client message"

serverMessage :: ByteString
serverMessage = "server message"

unixPath :: ByteString
unixPath = "Woum5ag3oohuaLee.socket"

clientUnixPath :: ByteString
clientUnixPath = "Io4meo0epoquashi.socket"

abstractPath :: ByteString
abstractPath = "/tmp/uth4Aechiereejae.socket"

clientAbstractPath :: ByteString
clientAbstractPath = "/tmp/FieNg4shamo4Thie.socket"

unixSocketStream :: IO (Socket Unix Stream Unix)
unixSocketStream = socket

unixSocketDatagram :: IO (Socket Unix Datagram Unix)
unixSocketDatagram = socket

testServerClientStream
    :: SocketAddress Unix
    -> (Socket Unix Stream Unix, Socket Unix Stream Unix)
    -> IO ()
testServerClientStream addr (server, client) = do
    bind server addr
    listen server 5
    serverRecv <- async $ do
        (peerSock, peerAddr) <- accept server
        r <- receive peerSock 4096 mempty
        send peerSock serverMessage mempty
        pure r
    connect client addr
    send client clientMessage mempty

    clientMessageReceived <- wait serverRecv
    serverMessageReceived <- receive client 4096 mempty
    clientMessageReceived @?= clientMessage
    serverMessageReceived @?= serverMessage

testServerClientDatagram
    :: SocketAddress Unix
    -> SocketAddress Unix
    -> (Socket Unix Datagram Unix, Socket Unix Datagram Unix)
    -> IO ()
testServerClientDatagram sAddr cAddr (server, client) = do
    bind server sAddr
    bind client cAddr
    serverRecv <- async $ do
        (r, peerAddr) <- receiveFrom server 4096 mempty
        sendTo server serverMessage mempty peerAddr
        pure r
    sendTo client clientMessage mempty sAddr

    clientMessageReceived <- wait serverRecv
    serverMessageReceived <- receive client 4096 mempty
    clientMessageReceived @?= clientMessage
    serverMessageReceived @?= serverMessage

