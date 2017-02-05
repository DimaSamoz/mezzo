

module Main where

import Test.Hspec

import PrimSpec

main :: IO ()
main = mapM_ hspec [ primSpec
                   ]
