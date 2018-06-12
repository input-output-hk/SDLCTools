

import            Test.Framework

import qualified  Test.Gtsim.Simulator as S

main :: IO ()
main = do
  defaultMain (S.tests)