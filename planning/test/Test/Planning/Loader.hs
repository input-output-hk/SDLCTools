{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Test.Gtsim.Loader

where




import            Control.Monad
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Monoid
import            Data.Ratio

import            Debug.Trace(trace)

import            Test.Framework (Test)
import            Test.Framework.Providers.QuickCheck2 (testProperty)

import            Test.QuickCheck (forAll, arbitrary, Property)
import            Test.QuickCheck.Gen (Gen, shuffle, sublistOf, choose, vectorOf)


import Gtsim.Types
import Gtsim.Invariants
import Gtsim.Simulator

tests :: [Test]
tests = []



