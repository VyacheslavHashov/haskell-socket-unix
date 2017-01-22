module Main where

import Test.Tasty (defaultMain, testGroup)

import Internal
import Platform

main :: IO ()
main = defaultMain $ testGroup "unix domain socket"
    [ groupUnixPathname
    , groupAbstractName ]

