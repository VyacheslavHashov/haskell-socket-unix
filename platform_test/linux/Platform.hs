module Platform where

import Data.Maybe (fromJust)
import Control.Exception (bracket, throwIO, try)
import Test.Tasty
import Test.Tasty.HUnit

import System.Socket
import System.Socket.Type.Stream
import System.Socket.Type.Datagram
import System.Socket.Family.Unix

import Internal

groupAbstractName :: TestTree
groupAbstractName = testGroup "Abstract path name"
    [ testCase "connect to non-existing path name" $ bracket
        unixSocketStream
        close
        (\s -> do
            r <- try $ connect s addr
            case r of
                Left e | e == eConnectionRefused -> return ()
                       | otherwise               -> throwIO e
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
    addr = fromJust $ socketAddressUnixAbstract abstractPath
    cAddr = fromJust $ socketAddressUnixAbstract clientAbstractPath
    closeSockets (server, client) = do
        close server
        close client

