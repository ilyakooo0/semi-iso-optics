{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{- |
Module      :  Control.Category.Structures
Description :  Structures in a category.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

This module defines some basic structures in a category in a more fine-grained
way then "Control.Arrow".

Unfortunately names in this module clash with "Control.Arrow".
-}
module Control.Category.Structures
    ( CatTrans(..)
    ) where

import           Control.Category
import           Prelude hiding (id, (.))


-- | A category transformer.
class CatTrans t where
    -- | Lift an arrow from the base category.
    clift :: Category cat => cat a b -> t cat a b

    {-# MINIMAL clift #-}
