{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module Planning.Loader

where

--import            Control.Monad.Fail
import qualified  Data.List as L
import qualified  Data.Map.Strict as M
import            Data.Maybe (mapMaybe)
import            Data.Random
import            Data.Random.Source
import            Data.Random.Internal.Source
import            Data.Random.Distribution.Beta (Beta, beta)
import            Data.Ratio
import qualified  Data.Text as T

import            Test.QuickCheck.Arbitrary
import            Test.QuickCheck.Gen (Gen, shuffle, frequency, choose, vectorOf, generate)

import Data.Word

import            Gtsim.Types
import            Gtsim.Simulator as S
import            Meas.Extract.Types

import System.IO.Unsafe (unsafePerformIO)


-- runRVar :: RandomSource m s => RVar a -> s -> m a

$(randomSource [d|
          instance RandomSource Gen Int where
--            getRandomPrimFrom :: s -> Prim t -> m t
            getRandomPrimFrom s PrimWord8 =             pure $ fromIntegral s -- s::Gen Word8
            getRandomPrimFrom s PrimWord16 =            pure $ fromIntegral s -- s::Gen Word16
            getRandomPrimFrom s PrimWord32 =            pure $ fromIntegral s -- s::Gen Word32
            getRandomPrimFrom s PrimWord64 =            pure $ fromIntegral s -- s::Gen Word64
            getRandomPrimFrom s PrimDouble =            pure $ fromIntegral s -- s::Gen Double
            getRandomPrimFrom s (PrimNByteInteger _) =  pure $ fromIntegral s -- s::Gen Integer
     |])
{-
$(randomSource [d|
          instance RandomSource Gen Rational where
--            getRandomPrimFrom :: s -> Prim t -> m t
            getRandomPrimFrom s PrimWord8 =             pure $ fromRational s -- s::Gen Word8
            getRandomPrimFrom s PrimWord16 =            pure $ fromRational s -- s::Gen Word16
            getRandomPrimFrom s PrimWord32 =            pure $ fromRational s -- s::Gen Word32
            getRandomPrimFrom s PrimWord64 =            pure $ fromRational s -- s::Gen Word64
            getRandomPrimFrom s PrimDouble =            pure $ fromRational s -- s::Gen Double
            getRandomPrimFrom s (PrimNByteInteger _) =  pure $ fromRational s -- s::Gen Integer
     |])
-}

$(randomSource [d|
          instance RandomSource IO Int where
--            getRandomPrimFrom :: s -> Prim t -> m t
            getRandomPrimFrom s PrimWord8 =             pure $ fromIntegral s -- s::Gen Word8
            getRandomPrimFrom s PrimWord16 =            pure $ fromIntegral s -- s::Gen Word16
            getRandomPrimFrom s PrimWord32 =            pure $ fromIntegral s -- s::Gen Word32
            getRandomPrimFrom s PrimWord64 =            pure $ fromIntegral s -- s::Gen Word64
            getRandomPrimFrom s PrimDouble =            pure $ fromIntegral s -- s::Gen Double
            getRandomPrimFrom s (PrimNByteInteger _) =  pure $ fromIntegral s -- s::Gen Integer
     |])


$(monadRandom [d|
         instance MonadRandom Gen where
             getRandomWord8 =         return 1
             getRandomWord16 =        choose (1, 2) -- return 1
             getRandomWord32 =        choose (1, 2) -- return 1
             getRandomWord64 =        return 1
             getRandomDouble =        choose (1, 2) -- return 1
             getRandomNByteInteger n = return 1
     |])

--betaGen :: Distribution Beta a => a -> a -> Gen a
--betaGen :: Distribution Beta a => a -> a -> Gen Float
--
--betaGen min max = do
--  (seed::Rational) <- arbitrary
--  runRVar (stdUniform) seed
--  --  runRVar (beta min max) seed

betaGen :: Float -> Float -> Gen Float
betaGen min max = do
  (seed::Int) <- arbitrary
  let e = go seed
  return $ (1.0 - e) * min  + e * max
  where
  go _ = unsafePerformIO $ sample $ beta 2.0 (5.0::Float)


{-


z _ = generate $ betaGen 10 20


--t = generate $ betaGen 1 (2::Float)
e :: Int -> IO Float
e seed = runRVar (stdUniform  ) (seed::Int)

a :: IO Float
a  = sample $ beta 2.0 (5.0::Float)

b _ = unsafePerformIO $ sample $ beta 2.0 (5.0::Float)

PrimWord8 :: Prim Word8
PrimWord16 :: Prim Word16
PrimWord32 :: Prim Word32
PrimWord64 :: Prim Word64
PrimDouble :: Prim Double
PrimNByteInteger :: !Int -> Prim Integer
-}

iohkResources =
  [ MkResource "Squad 1"          (stdResourceEfficiency 2.5) parTaskGen
  , MkResource "Squad 2"          (stdResourceEfficiency 1.0) parTaskGen
  , MkResource "Squad 3"          (stdResourceEfficiency 2.5) parTaskGen
  , MkResource "Squad 4"          (stdResourceEfficiency 2.5) parTaskGen
  , MkResource "New Wallet Squad" (stdResourceEfficiency 5.0) parTaskGen
  , MkResource "Core Squad"       (stdResourceEfficiency 6.0) parTaskGen
  , MkResource "Network Squad"    (stdResourceEfficiency 3.0) parTaskGen
  ]
  where
  parTaskGen = choose (1, 5)

iohkResourceMap = M.fromList $ map (\r -> (rId r, r)) iohkResources


resourceEfficiency :: [(Int, Float)] -> (Int -> ManDaysPerDay)
resourceEfficiency effs = go
  where
  go 0 = go 1
  go i = (M.fromList $ map (\(i, e) -> (i, toRational e)) effs) M.! i

stdResourceEfficiency :: Float -> (Int -> ManDaysPerDay)
stdResourceEfficiency baseEff =
  resourceEfficiency effs
  where
  effs = zipWith (\i f -> (i, let b = fromIntegral i * (1+f) in baseEff / b)) [1..] switchPercent
  switchPercent = L.take 10 $ L.repeat 0.0
  --switchPercent = [0, 0.1, 0.15, 0.20, 0.30, 0.45, 0.7, 1, 1.5, 2.0, 2.0, 3.0]


initialize :: [Resource] -> [YtIssue] -> (SimState, ProblemDefinition)
initialize resources ytIssues =
  S.initialize tasks resources dependencies runningAssignmentGen
  where
  tasks = mapMaybe mkTask ytIssues
  dependencies = M.fromList $ map (\t -> (tId t, [])) tasks
  runningAssignmentGen = pure True -- frequency [(40, pure True), (60, pure False)]


mkTask :: YtIssue -> Maybe Task
mkTask (MkYtIssue {..}) = do
  romMandays <- _ytiROMManday
  let tId = T.unpack _ytiIssueId
  let tProjectId = T.unpack _ytiProject
  let tManDaysGen = romMandaysToGen romMandays
  let tCanBeDoneBy = map T.unpack _ytiPotentialSquad
  if L.null tCanBeDoneBy
    then fail ""
    else return $ MkTask {..}






{-


{-}
data ProblemDefinition = MkProblemDefinition
  { pdTasks                  :: M.Map TaskId Task         -- ^ All initial Tasks
  , pdResources              :: M.Map ResourceId Resource -- ^ All available Resources
  , pdTasksPerResource       :: [(ResourceId, TaskId)]    -- ^ (R, T): the resource R can work on task T
  , pdDependencies           :: M.Map TaskId [TaskId]     -- ^ The dependent Task (key) must start after all of the pre-requisites Tasks (values) are completed.
  , pdRunningAssignmentGen   :: Gen Bool                  -- ^ Generator of Running/Waiting flag. Corresponds to the WaitingAssignmentProb in the specs.
  }
-}

data Resource = MkResource
  { rId                       :: ResourceId           -- ^ Identifier of the Resource
  , rEfficiency               :: Int -> ManDaysPerDay -- ^ Efficiency wrt the number of concurrent running Assignment. rEfficiency 0 == rEfficiency 1
  , rNbActiveAssignmentsGen   :: Gen Int              -- ^ Generator for the number of concurrent Active Assignments for this Resource.
  }
data Task = MkTask
  { tId               :: TaskId         -- ^ Identifier of the Task
  , tProjectId        :: ProjectId      -- ^ Project the Task belongs to
  , tManDaysGen       :: Gen ManDays    -- ^ Generator of effort in man.days for this Task
  , tCanBeDoneBy      :: [ResourceId]   -- ^ Resources that can work on this Task
  }

data YtIssue = MkYtIssue
  { _ytiIssueId           :: T.Text
  , _ytiCreated           :: Int
  , _ytiProject           :: T.Text
  , _ytiNumber            :: Int
  , _ytiState             :: StateValue
  , _ytiWait              :: WaitValue
  , _ytiDueDate           :: Int
  , _ytiROMManday         :: Maybe ROMMandaysValue
  , _ytiPPriorities       :: (Int, Int, Int)
  , _ytiSquad             :: Maybe T.Text
  , _ytiOwner             :: Maybe T.Text
  , _ytiPotentialSquad    :: [T.Text]
  , _ytiTargetVersions    :: [T.Text]
  , _ytiLinks             :: [(LinkType, T.Text)]
  , _ytiChanges           :: [(Int, [ValueChange])]
  , _ytiStateTransitions  :: StateTransitions
  , _ytiBlockedDays       :: Integer
  , _ytiErrors            :: [String]
  }
  deriving (Show, Generic, NFData)-}


romMandaysToGen :: ROMMandaysValue -> Gen ManDays
romMandaysToGen Days      = betaGen 0.5  4.0    >>= (return . toRational)
romMandaysToGen Weeks     = betaGen 3.0  13.0   >>= (return . toRational)
romMandaysToGen Months    = betaGen 12.0 45.0   >>= (return . toRational)
romMandaysToGen Quarters  = betaGen 40.0 100.0  >>= (return . toRational)

--romMandaysToGen Days      = choose (0.5 , 4.0::Float)    >>= (return . toRational)
--romMandaysToGen Weeks     = choose (3.0 , 13.0::Float)   >>= (return . toRational)
--romMandaysToGen Months    = choose (12.0, 45.0::Float)   >>= (return . toRational)
--romMandaysToGen Quarters  = choose (40.0, 100.0::Float)  >>= (return . toRational)

