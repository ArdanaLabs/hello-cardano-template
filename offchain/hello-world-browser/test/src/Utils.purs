module Utils where

import Prelude

import Data.Const (Const)
import Data.Foldable (sequence_)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Mote (Plan, foldPlan, planT)
import Test.Spec (Spec, describe, it, parallel, pending)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.Spec.Runner as SpecRunner
import TestM (TestPlanM)

data RunningMode
  = InParallel
  | InSequence

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret' :: SpecRunner.Config -> TestPlanM Unit -> RunningMode -> Aff Unit
interpret' config spif runningMode = do
  plan <- planT spif
  case runningMode of
    InParallel -> runSpec' config [ consoleReporter ] $ parallel $ go plan
    InSequence -> runSpec' config [ consoleReporter ] $ go plan
  where
  go :: Plan (Const Void) (Aff Unit) -> Spec Unit
  go =
    foldPlan
      (\x -> it x.label $ liftAff x.value)
      pending
      (\x -> describe x.label $ go x.value)
      sequence_
