{-# language OverloadedStrings #-}
module Main where

import Control.Exception (bracket, catch, throwIO, try)
import Data.Maybe (fromJust)
import Data.String (IsString)
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
        (\(server, client) -> do
            let addr = fromJust $ socketAddressUnixPath unixPath
            bind server addr
            listen server 5
            connect client addr
        )
    ]

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
        (\(server, client) -> do
            let addr = fromJust $ socketAddressUnixAbstract abstractPath
            bind server addr
            listen server 5
            connect client addr
        )
    ]

