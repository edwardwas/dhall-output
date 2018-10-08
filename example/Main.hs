{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Dhall.Output

import           Data.Proxy   (Proxy (..))
import           Data.Text    (Text)
import qualified Data.Text.IO as T
import           Dhall
import qualified Generics.SOP as SOP

data FooBar = Foo {foo :: Natural} | Bar {bar :: Bool, bazz :: [Text]}
  deriving (Eq,Show,Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo,Interpret)

data TestProduct = TestProduct {testList :: [Bool], testNum :: Natural}
  deriving (Eq,Show,Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

newtype TestNewtype = TestNewtype {bleep :: Text}
  deriving (Eq,Show,Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo,Interpret)

outputType :: OutputType
outputType =
  makeOutputType (Proxy @FooBar) <>
  makeOutputType (Proxy @TestProduct) <>
  makeOutputType (Proxy @TestNewtype)

main :: IO ()
main = do
  T.writeFile "example/types.dhall" $ prettyOutputType outputType
  res <- detailed $ input (auto @(FooBar,TestNewtype)) "./example/test.dhall"
  print res
