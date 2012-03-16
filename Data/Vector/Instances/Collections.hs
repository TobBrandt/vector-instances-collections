{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Vector.Instances.Collections () where

import Data.Vector.Instances.Collections.Internal

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Primitive as VP

import Data.Collections 
  ( Foldable(..)
  , Unfoldable(..)
  , Collection(..)
  , Sequence(..)
  , Indexed(..)
  )

declareInstances 
    [''Foldable, ''Unfoldable, ''Collection, ''Sequence, ''Indexed] 
    [''V.Vector, ''VU.Vector, ''VP.Vector, ''VS.Vector]
