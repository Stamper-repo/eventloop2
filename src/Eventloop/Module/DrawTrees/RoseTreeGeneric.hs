{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts, DeriveGeneric,
             DeriveAnyClass, TypeSynonymInstances, FlexibleInstances,
             ScopedTypeVariables, StandaloneDeriving #-}
module Eventloop.Module.DrawTrees.RoseTreeGeneric
    ( ToRoseTree (..)
    , genericToRoseTree
    ) where

import GHC.Generics
import Eventloop.Module.DrawTrees.Types

emptyNode = RoseNode "" []

cleanTree = concatMap mergeProduct
          . filter (/= emptyNode)

mergeProduct (RoseNode "" xs) = xs
mergeProduct r                = [r]

-- | Convert to a 'RoseTree'
class ToRoseTree a where
  -- | Convert to a 'RoseTree'
  toRoseTree :: a -> RoseTree
  default toRoseTree :: (Generic a, GToRoseTree (Rep a)) => a -> RoseTree
  toRoseTree = genericToRoseTree

-- | A version of 'toRoseTree' that works for any data type that has an
-- instance for 'Generic'
genericToRoseTree :: (Generic a, GToRoseTree (Rep a)) => a -> RoseTree
genericToRoseTree = gtoRoseTree . from

class GToRoseTree f where
  gtoRoseTree :: f a -> RoseTree

-- constants
instance ToRoseTree c => GToRoseTree (K1 i c) where
  gtoRoseTree = toRoseTree . unK1

-- meta
instance {-# OVERLAPPABLE #-} GToRoseTree f => GToRoseTree (M1 i c f) where
  gtoRoseTree = gtoRoseTree . unM1

-- constructors
instance (Constructor c, GToRoseTree f) => GToRoseTree (C1 c f) where
  gtoRoseTree = RoseNode (conName (undefined :: t c f p))
              . cleanTree
              . (:[])
              . gtoRoseTree
              . unM1

-- unit
instance GToRoseTree U1 where
  gtoRoseTree U1 = emptyNode

-- void
instance GToRoseTree V1 where
  gtoRoseTree _ = emptyNode

-- product
instance (GToRoseTree f, GToRoseTree g) => GToRoseTree (f :*: g) where
  gtoRoseTree (f1 :*: g1) = RoseNode "" (cleanTree [gtoRoseTree f1, gtoRoseTree g1])

-- sum
instance (GToRoseTree f, GToRoseTree g) => GToRoseTree (f :+: g) where
  gtoRoseTree (L1 f1) = gtoRoseTree f1
  gtoRoseTree (R1 g1) = gtoRoseTree g1

instance ToRoseTree Int where
  toRoseTree i = RoseNode (show i) []

instance ToRoseTree Integer where
  toRoseTree i = RoseNode (show i) []

instance ToRoseTree Float where
  toRoseTree f = RoseNode (show f) []

instance ToRoseTree Double where
  toRoseTree d = RoseNode (show d) []

instance ToRoseTree Char where
  toRoseTree c = RoseNode [c] []

deriving instance ToRoseTree Bool
deriving instance ToRoseTree Ordering
deriving instance (ToRoseTree l, ToRoseTree r) => ToRoseTree (Either l r)
deriving instance ToRoseTree a => ToRoseTree (Maybe a)

instance {-# OVERLAPPABLE #-} ToRoseTree a => ToRoseTree [a] where
  toRoseTree = RoseNode "" . cleanTree . map toRoseTree

instance ToRoseTree String where
  toRoseTree s = RoseNode s []
