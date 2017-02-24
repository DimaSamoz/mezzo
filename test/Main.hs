

module Main where

import Test.Hspec

import PrimSpec
import TypeSpec

main :: IO ()
main = mapM_ hspec [ primSpec
                --    , typeSpec
                   ]
