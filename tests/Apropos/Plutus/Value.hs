module Apropos.Plutus.Value (
  --valueGenSelfTests,
  MultiValueProp,
  MultiValue,
  ListModel(..),
  ) where

import Apropos

import Apropos.Plutus.SingletonValue

type MultiValue = [SingletonValue]

data ListModel subModel
  = Empty
  | Len1
  | Len2
  | Len3
  | Long
  | At0 subModel
  | At1 subModel
  | At2 subModel
  | AtRest subModel
  deriving stock (Eq,Ord,Show)

instance Enumerable a => Enumerable (ListModel a) where
  enumerated = undefined

type MultiValueProp = ListModel SingletonValue

instance LogicalModel model => LogicalModel (ListModel model) where
  logic = ExactlyOne
    [ Var Empty
    , Var Len1 :&&: (At0 <$> logic)
    , Var Len2 :&&: (At0 <$> logic) :&&: (At1 <$> logic)
    , Var Len3 :&&: (At0 <$> logic) :&&: (At1 <$> logic) :&&: (At2 <$> logic)
    , Var Long :&&: (At0 <$> logic) :&&: (At1 <$> logic) :&&: (At2 <$> logic) :&&: (AtRest <$> logic)
    ]

instance HasLogicalModel prop m => HasLogicalModel (ListModel prop) [m] where
  satisfiesProperty Empty xs = null xs
  satisfiesProperty Len1 xs = length xs == 1
  satisfiesProperty Len2 xs = length xs == 2
  satisfiesProperty Len3 xs = length xs == 3
  satisfiesProperty Long (_:_:_:_:_) = True
  satisfiesProperty Long _ = False

  satisfiesProperty (At0 m) xs = satisfiesProperty m (head xs)
  satisfiesProperty (At1 m) xs = satisfiesProperty m (xs !! 1)
  satisfiesProperty (At2 m) xs = satisfiesProperty m (xs !! 2)

  satisfiesProperty (AtRest m) xs = all (satisfiesProperty m) (drop 3 xs)

