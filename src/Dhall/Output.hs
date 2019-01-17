{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

{-|
Module      : Dhall.Output
Description : Output dhall types to file
Copyright   : (c) Ed Wastell, 2018
License     : MIT
Maintainer  : ed@wastell.co.uk
Stability   : experimental
Portability : POSIX

Write Haskell types to dhall files. Consult the readme for more info
-}

module Dhall.Output
  ( OutputType
  , makeOutputType
  , makeOutputTypeT
  , prettyOutputType
  ) where

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Semigroup
#endif
import           Data.Proxy                (Proxy (..))
import qualified Data.Text                 as T
import qualified Data.Text.Prettyprint.Doc as Pretty
import           Dhall                     hiding (Generic)
import           Dhall.Core                (Expr (..))
import qualified Dhall.Map                 as DM
import           Dhall.Parser              (Src (..))
import           Dhall.TypeCheck           (X (..))
import           Generics.SOP              hiding (Record)
import qualified Generics.SOP              as SOP

fieldHelper ::
     forall a. Interpret a
  => FieldInfo a
  -> (T.Text, Expr Src X)
fieldHelper (FieldInfo name) = (T.pack name, expected $ auto @a)

constructorHelper ::
     forall as. (All Interpret as)
  => ConstructorInfo as
  -> (T.Text, Expr Src X)
constructorHelper (SOP.Record rname fields) =
  ( T.pack rname
  , Record $
    DM.fromList $ hcollapse $ hcmap (Proxy @Interpret) (K . fieldHelper) fields)
constructorHelper (Constructor name) = (T.pack name, Record mempty)
constructorHelper (Infix _ _ _) = error "Infix not done"

makeUnionType ::
     forall a. (All2 Interpret (Code a), HasDatatypeInfo a, SListI (Code a))
  => Proxy a
  -> Expr Src X
makeUnionType p =
  case shape :: Shape (Code a) of
    ShapeCons ShapeNil ->
      case datatypeInfo p of
        ADT _ _ cis             -> snd $ constructorHelper $ hd cis
        Newtype _ _ constructor -> snd $ constructorHelper constructor
    _ ->
      case datatypeInfo p of
        ADT _ _ cis ->
          Union $
          DM.fromList $
          hcollapse
            (hcmap (Proxy @(All Interpret)) (K . constructorHelper) cis :: NP (K ( T.Text
                                                                                 , Expr Src X)) (Code a))
        Newtype _ _ constructor -> snd $ constructorHelper constructor

-- | The representation of a type to write out. We can combine multiple with the `Semigroup` and `Monoid` instances
newtype OutputType = OutputType
  { outputTypes :: DM.Map T.Text (Expr Src X)
  } deriving (Eq,Show)
    deriving newtype (Semigroup,Monoid)

-- | Like `makeOutputTypeT`, but we use the name of the datatype as the name
makeOutputType :: (All2 Interpret (Code a), HasDatatypeInfo a) => Proxy a -> OutputType
makeOutputType p = makeOutputTypeT (T.pack $ datatypeName $ datatypeInfo p) p

-- | Create an output type. The first input is the name of the type to use in the output file. The second is a `Proxy` of the type we wish to store.
makeOutputTypeT ::
     forall a. (All2 Interpret (Code a), HasDatatypeInfo a)
  => T.Text
  -> Proxy a
  -> OutputType
makeOutputTypeT n p =
  let ut = makeUnionType p
  in OutputType (DM.singleton n ut)

-- | Convert an `OutputType` to a `Text`. This will be nicely formated
prettyOutputType :: OutputType -> Text
prettyOutputType (OutputType t) = T.pack $ show $ Pretty.pretty $ RecordLit t
