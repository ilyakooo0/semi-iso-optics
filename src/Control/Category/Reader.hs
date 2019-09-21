{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Control.Category.Reader
Description :  Reader category transformer.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Provides a Reader category transformer.
-}
module Control.Category.Reader (
    ReaderCT(..)
    ) where

import Control.Arrow
import Control.Category
import Control.Category.Structures
import Control.Lens.Iso
import Control.Lens.SemiIso
import Control.SIArrow
import Prelude hiding (id, (.))

newtype ReaderCT env cat a b = ReaderCT { runReaderCT :: env -> cat a b }

instance CatTrans (ReaderCT env) where
    clift = ReaderCT . const

instance Category cat => Category (ReaderCT env cat) where
    id = clift id
    ReaderCT f . ReaderCT g = ReaderCT $ \x -> f x . g x

instance Arrow cat => Arrow (ReaderCT env cat) where
    arr f = ReaderCT $ \_ -> arr f
    ReaderCT f *** ReaderCT g = ReaderCT $ \x -> f x *** g x

instance ArrowChoice cat => ArrowChoice (ReaderCT env cat) where
    ReaderCT f +++ ReaderCT g = ReaderCT $ \x -> f x +++ g x

instance ArrowZero cat => ArrowZero (ReaderCT env cat) where
    zeroArrow = clift zeroArrow

instance ArrowPlus cat => ArrowPlus (ReaderCT env cat) where
    ReaderCT f <+> ReaderCT g = ReaderCT $ \x -> f x <+> g x

instance SIArrow cat => SIArrow (ReaderCT env cat) where
    siarr = clift . siarr
    sibind ai = ReaderCT $ \env -> sibind
        (iso id (flip runReaderCT env) . cloneSemiIso ai . iso (flip runReaderCT env) id)
