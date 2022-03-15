module Apropos.Plutus.List (
    ListModel,
    Satable (sats),
) where

import Apropos

import Control.Lens (Lens, lens)
import Control.Monad (join)

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
    deriving stock (Eq, Ord, Show)

instance Enumerable a => Enumerable (ListModel a) where
    enumerated = [Empty, Len1, Len2, Len3, Long] ++ [c v | v <- enumerated, c <- [At0, At1, At2, AtRest]]

instance LogicalModel model => LogicalModel (ListModel model) where
    logic =
        ExactlyOne [Var Empty, Var Len1, Var Len2, Var Len3, Var Long]
            :&&: ( Var Empty
                    :->: None [Var (at s) | at <- [At0, At1, At2, AtRest], s <- enumerated]
                 )
            :&&: ( Var Len1
                    :->: ( All [at <$> logic | at <- [At0]]
                            :&&: None [Var (at s) | at <- [At1, At2, AtRest], s <- enumerated]
                         )
                 )
            :&&: ( Var Len2
                    :->: ( All [at <$> logic | at <- [At0, At1]]
                            :&&: None [Var (at s) | at <- [At2, AtRest], s <- enumerated]
                         )
                 )
            :&&: ( Var Len3
                    :->: ( All [at <$> logic | at <- [At0, At1, At2]]
                            :&&: None [Var (at s) | at <- [AtRest], s <- enumerated]
                         )
                 )
            :&&: ( Var Long
                    :->: All [at <$> logic | at <- [At0, At1, At2, AtRest]]
                 )

class LogicalModel a => Satable a where
    sats :: [a]

instance HasLogicalModel prop m => HasLogicalModel (ListModel prop) [m] where
    satisfiesProperty Empty xs = null xs
    satisfiesProperty Len1 xs = length xs == 1
    satisfiesProperty Len2 xs = length xs == 2
    satisfiesProperty Len3 xs = length xs == 3
    satisfiesProperty Long (_ : _ : _ : _ : _) = True
    -- I think this is faster than length xs >= 4
    satisfiesProperty Long _ = False
    satisfiesProperty (At0 m) xs = satisfiesProperty m (head xs)
    satisfiesProperty (At1 m) xs = satisfiesProperty m (xs !! 1)
    satisfiesProperty (At2 m) xs = satisfiesProperty m (xs !! 2)
    satisfiesProperty (AtRest m) xs = all (satisfiesProperty m) (drop 3 xs)

instance
    ( Satable prop
    , HasPermutationGenerator prop model
    , HasParameterisedGenerator prop model
    ) =>
    HasPermutationGenerator (ListModel prop) [model]
    where
    generators =
        let satSubVars :: [prop]
            satSubVars = sats
            {-
            case listToMaybe (solveAll (logic :: Formula prop)) of
               Just sol -> M.keys . M.filter id $ sol
               Nothing -> error "subModel for list couldn't be solved"
              -}
            satSubForm :: Formula prop
            satSubForm = foldl (:&&:) Yes (Var <$> satSubVars)
            at0 :: Abstraction prop model (ListModel prop) [model]
            at0 =
                Abstraction
                    { abstractionName = "At0"
                    , propertyAbstraction = abstractsProperties At0
                    , modelAbstraction = at' 0
                    }
            at1 :: Abstraction prop model (ListModel prop) [model]
            at1 =
                Abstraction
                    { abstractionName = "At1"
                    , propertyAbstraction = abstractsProperties At1
                    , modelAbstraction = at' 1
                    }
            at2 :: Abstraction prop model (ListModel prop) [model]
            at2 =
                Abstraction
                    { abstractionName = "At2"
                    , propertyAbstraction = abstractsProperties At2
                    , modelAbstraction = at' 2
                    }
            atRest :: Abstraction prop model (ListModel prop) [model]
            atRest =
                Abstraction
                    { abstractionName = "AtRest"
                    , propertyAbstraction = abstractsProperties AtRest
                    , modelAbstraction = undefined
                    -- TODO this can work with upstream change
                    --lens (drop 3) (\xs ys -> take 3 xs ++ ys) . each
                    }
            addMorphism = \xs -> do
                x <- genSatisfying satSubForm
                pure $ xs ++ [x]
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
                    , contract = remove Empty >> add Len1 >> addAll (At0 <$> satSubVars)
                    , morphism = addMorphism
                    }
                , Morphism
                    { name = "add1"
                    , match = Var Len1
                    , contract = remove Len1 >> add Len2 >> addAll (At1 <$> satSubVars)
                    , morphism = addMorphism
                    }
                , Morphism
                    { name = "add2"
                    , match = Var Len2
                    , contract = remove Len2 >> add Len3 >> addAll (At2 <$> satSubVars)
                    , morphism = addMorphism
                    }
                , Morphism
                    { name = "addRest"
                    , match = Var Len3
                    , contract = remove Len3 >> add Long >> addAll (AtRest <$> satSubVars)
                    , morphism = \xs -> do
                        ys <- list (linear 1 100) (genSatisfying satSubForm)
                        pure $ xs ++ ys
                    }
                ]
         in join
                [ abstract abstraction <$> generators
                | abstraction <- [at0, at1, at2, atRest]
                ]
                ++ lengthMorphisms

-- TODO better error checking on this lens
at' :: Int -> Lens [a] [a] a a
at' n = lens (!! n) (\xs x -> take (n -1) xs ++ [x] ++ drop n xs)

instance
    ( Satable prop
    , HasPermutationGenerator prop model
    , HasParameterisedGenerator prop model
    ) =>
    HasParameterisedGenerator (ListModel prop) [model]
    where
    parameterisedGenerator = buildGen baseGen

baseGen :: Gen [model]
baseGen = pure []
