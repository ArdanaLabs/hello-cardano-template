module Apropos.Plutus.Value (
  --valueGenSelfTests,
  MultiValueProp,
  MultiValue,
  ListModel(..),
  ) where

import Apropos

import Apropos.Plutus.SingletonValue
import Control.Monad (join)
import Control.Lens(Lens,lens)

type MultiValue = [SingletonValue]

-- TODO can/should this use Nats?
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
  enumerated = [Empty,Len1,Len2,Len3,Long] ++ [ c v | v <- enumerated , c <- [At0,At1,At2,AtRest]]

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
  -- I think this is faster than length xs >= 4
  satisfiesProperty Long _ = False

  satisfiesProperty (At0 m) xs = satisfiesProperty m (head xs)
  satisfiesProperty (At1 m) xs = satisfiesProperty m (xs !! 1)
  satisfiesProperty (At2 m) xs = satisfiesProperty m (xs !! 2)

  satisfiesProperty (AtRest m) xs = all (satisfiesProperty m) (drop 3 xs)

instance HasPermutationGenerator prop model => HasPermutationGenerator (ListModel prop) [model] where
  generators = let
    at0 :: Abstraction prop model (ListModel prop) [model]
    at0 = Abstraction
      { abstractionName = "At0"
      , propertyAbstraction = abstractsProperties At0
      , modelAbstraction = at' 0
      }
    at1 :: Abstraction prop model (ListModel prop) [model]
    at1 = Abstraction
      { abstractionName = "At1"
      , propertyAbstraction = abstractsProperties At1
      , modelAbstraction = at' 1
      }
    at2 :: Abstraction prop model (ListModel prop) [model]
    at2 = Abstraction
      { abstractionName = "At2"
      , propertyAbstraction = abstractsProperties At2
      , modelAbstraction = at' 2
      }
    atRest :: Abstraction prop model (ListModel prop) [model]
    atRest = Abstraction
      { abstractionName = "AtRest"
      , propertyAbstraction = abstractsProperties AtRest
      , modelAbstraction = undefined
          --shouldBeSomethingLike =
           -- lens (drop 3) (\xs ys -> take 3 xs ++ ys)
           -- this lens part isn't quite right
           -- . each
           -- this requires applicative
           -- this may be easiest to make work
           -- by changing apropos
      }
    -- TODO
    lengthMorphisms =
      [ Morphism
        { name = "empty"
        , match = Yes
        , contract = clear >> add Empty
        , morphism = \_ -> pure []
        }
      , Morphism
        { name = "add0"
        , match = Var Empty
        , contract = remove Empty >> add Len1
        -- TODO At0 properties
        -- and actual morphism
        , morphism = undefined
        }
        -- TODO add1 add2 and addRest edges
      ]
                in join [abstract abstraction <$> generators
                        | abstraction <- [at0,at1,at2,atRest] ]
                      ++ lengthMorphisms

-- TODO better error checking on this lens
at' :: Int -> Lens [a] [a] a a
at' n = lens (!! n) (\xs x -> take (n-1) xs ++ [x] ++ drop n xs)


instance HasPermutationGenerator prop model => HasParameterisedGenerator (ListModel prop) [model] where
  parameterisedGenerator = buildGen baseGen
    where
      baseGen :: Gen [model]
      baseGen = pure []

