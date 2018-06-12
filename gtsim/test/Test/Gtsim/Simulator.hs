{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Test.Gtsim.Simulator

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
tests = [
   testProperty "testInvariantsOnly"                  testInvariantsOnly
   , testProperty "testOneResource"                     testOneResource
   , testProperty "testMultipleResources"               testMultipleResources
   , testProperty "testMultipleResourcesWithParallism"  testMultipleResourcesWithParallism
   , testProperty "testIndependentProjects"             testIndependentProjects
   , testProperty "testInvariantsOnly"                  testInvariantsOnly
    ]



tol :: Double
tol = 0.01


getFinalEndTime :: SimState -> Day
getFinalEndTime MkSimState{..} = (caEndTime . L.head) sstCompletedAssignments

inv :: SimState -> [(Bool, String)]

inv = simStateInv

finalInv :: ProblemDefinition -> SimState -> [(Bool, String)]
finalInv = finalSimStateAdditionalInv

--inv _ = []
--finalInv _ _ = []

testInvariantsOnly :: Property
testInvariantsOnly =
  forAll gen prop
  where
  gen = do
    let deltaTime = 1
    nbResources <- choose (1::Int, 5)
    resources <- mapM (\rId -> do
      maxNbParallelTasks <- choose (1, 4)
      resEffsMap <- vectorOf maxNbParallelTasks (choose (0.5, 2.0::Float))
                    >>= (return . M.fromList . L.zip [1 .. maxNbParallelTasks] )
      let resEffFun i = realToFrac $ resEffsMap M.! i
      return $ MkResource ("R-" ++ show rId) resEffFun (choose (1, maxNbParallelTasks))
      ) [1 .. nbResources]

    nbTasks <- choose (1, 100)
    tasks <- genTasks "PRJ" resources (return $ choose (10.0, 20.0::Float) >>= (return . realToFrac)) 0 nbTasks >>= genRemoveResources

    dependencies <- genDependencies (nbTasks `div` 2) (L.map tId tasks)
    let (simState, problemDefinition) = initialize tasks resources dependencies arbitrary

    -- time to run the simulation
    simRes <- runSimulation inv finalInv problemDefinition deltaTime simState

    return simRes

  prop res =
    case res of
      Right _ -> True
      Left msg -> trace (show msg) False

testOneResource :: Property
testOneResource =
  forAll gen prop
  where
  gen = do
    let deltaTime = 1
    (resEff::Rational) <-  choose (0.5 , 2.0::Float) >>= (return . realToFrac)
    let resEffFun = const resEff
    let resource = MkResource "R1" resEffFun (return 1)
    (manDays::Rational) <- choose (10, 20::Int)  >>= (return . realToFrac)
    let manDaysGenGen = return $ return manDays
    nbTasks <- choose (1, 10)
    tasks <- genTasks "PRJ" [resource] manDaysGenGen 0 nbTasks
    let endTime = manDays * (fromIntegral nbTasks) / resEff
    -- Each resource can take any task, so dependencies have no effect
    dependencies <- genDependencies (nbTasks `div` 2) (L.map tId tasks)
    let (simState, problemDefinition) = initialize tasks [resource] dependencies (return True)

    -- time to run the simulation
    simRes <- runSimulation inv finalInv  problemDefinition deltaTime simState
    return (simRes, endTime)

  prop (res, expectedEndTime) =
    case res of
      Right simState -> abs (getFinalEndTime simState - expectedEndTime) < (realToFrac tol)
      Left msg -> trace (show msg) False

testMultipleResources :: Property
testMultipleResources =
  forAll gen prop
  where
  gen = do
    let deltaTime = 1
    (resEff::Rational) <-  choose (0.5 , 2.0::Float) >>= (return . realToFrac)
    let resEffFun = const resEff
    nbResources <- choose (2, 6)
    let resources =  [MkResource ("R-"++show i) resEffFun (return 1) | i <- [1 .. nbResources]]
    let nbTasks = nbResources * 10
    (manDays::Rational) <- choose (10, 20::Int)  >>= (return . realToFrac)
    let manDaysGenGen = return $ return manDays
    tasks <- genTasks "PRJ" resources manDaysGenGen 0 nbTasks
    let endTime = manDays * (fromIntegral nbTasks) / resEff / (fromIntegral nbResources)

    -- Dependencies matter here,unless we have one chain of deps per resources (Todo)
    dependencies <- genDependencies 0 (L.map tId tasks)
    let (simState, problemDefinition) = initialize tasks resources dependencies (return True)

    -- time to run the simulation
    simRes <- runSimulation inv finalInv  problemDefinition deltaTime simState
    return (simRes, endTime)

  prop (res, expectedEndTime) =
    case res of
      Right simState -> abs (getFinalEndTime simState - expectedEndTime) < (realToFrac tol)
      Left msg -> trace (show msg) False


testMultipleResourcesWithParallism :: Property
testMultipleResourcesWithParallism =
  forAll gen prop
  where
  gen = do
    let deltaTime = 1 % 2
    maxNbParallelTasks <- choose (2, 4)
    (resEff::Rational) <-  choose (0.5 , 2.0::Float) >>= (return . realToFrac)
    let resEffFun = const resEff

    nbResources <- choose (1, 5)
    let resources =  [MkResource ("R-"++show i) resEffFun (return maxNbParallelTasks) | i <- [1 .. nbResources]]
    let nbTasks = nbResources * maxNbParallelTasks * 10
    (manDays::Rational) <- choose (10, 20::Int)  >>= (return . realToFrac)
    let manDaysGenGen = return $ return manDays
    tasks <- genTasks "PRJ" resources manDaysGenGen 0 nbTasks
    let endTime = manDays * (fromIntegral nbTasks) / resEff / (fromIntegral nbResources) / (fromIntegral maxNbParallelTasks)
    -- Dependencies matter here,unless we have one chain of deps per resources (Todo)
    dependencies <- genDependencies 0 (L.map tId tasks)
    let (simState, problemDefinition) = initialize tasks resources dependencies (return True)

    -- it is time to run the simulation
    simRes <- runSimulation inv finalInv  problemDefinition deltaTime simState
    return (simRes, endTime)

  prop (res, expectedEndTime) =
    case res of
      Right simState -> abs (getFinalEndTime simState - expectedEndTime) < (realToFrac tol)
      Left msg -> trace (show msg) False


{-
multiple independent projects, each with its own tasks and resource.
Each project has the same end date
-}

testIndependentProjects :: Property
testIndependentProjects =
  forAll gen prop
  where
  gen = do
    let deltaTime = 1
    nbProjects <- choose (1, 6)
    res <- mapM genProject [1::Int .. nbProjects]
    let globalEndTime = L.maximum $ L.map snd res
    let globalProblemDefinition = L.foldl' mergeProblemDefinitions emptyProblemDefinition (L.map (snd . fst) res)
    let globalSimState = L.foldl' mergeSimStates emptySimState (L.map (fst . fst) res)

    -- time to run the simulation
    simRes <- runSimulation inv finalInv  globalProblemDefinition deltaTime globalSimState
    return (simRes, globalEndTime)

  genProject projectId = do
    (resEff'::Rational) <-  choose (0.5 , 2.0::Float) >>= (return . realToFrac)
    let resEffFun = const resEff'
    nbResources <- choose (1, 6)
    let resources =  [MkResource ("R-"++show projectId++"-"++show i) resEffFun (return 1) | i <- [1 .. nbResources]]
    let nbTasks = nbResources * 10
    (manDays::Rational) <- choose (10, 20::Int)  >>= (return . realToFrac)
    let manDaysGenGen = return $ return manDays
    let endTime = manDays * (fromIntegral nbTasks) / resEff' / (fromIntegral nbResources)
    tasks <- genTasks ("PRJ-"++show projectId) resources manDaysGenGen 0 nbTasks
    -- Dependencies matter here,unless we have one chain of deps per resources (Todo)
    dependencies <- genDependencies 0 (L.map tId tasks)
    let (simState, problemDefinition) = initialize tasks resources dependencies (return True)
    return ((simState, problemDefinition), endTime)

  prop (res, expectedEndTime) =
    case res of
      Right simState -> abs (getFinalEndTime simState - expectedEndTime) < (realToFrac tol)
      Left msg -> trace (show msg) False

emptySimState :: SimState
emptySimState = MkSimState 0 [] []

emptyProblemDefinition :: ProblemDefinition
emptyProblemDefinition = MkProblemDefinition M.empty M.empty [] M.empty (return True)

mergeSimStates :: SimState -> SimState -> SimState
mergeSimStates simState1 simState2 =
  MkSimState  newSstCurrentTime newSstActiveAssignments newSstCompletedAssignments
  where
  newSstCurrentTime = sstCurrentTime simState1
  newSstActiveAssignments = sstActiveAssignments simState1 ++ sstActiveAssignments simState2
  newSstCompletedAssignments = sstCompletedAssignments simState1 ++ sstCompletedAssignments simState2

mergeProblemDefinitions :: ProblemDefinition -> ProblemDefinition -> ProblemDefinition
mergeProblemDefinitions pd1 pd2 =
  MkProblemDefinition  newPdTasks newPdResources newPdTasksPerResource newPdDependencies newPdRunningAssignmentGen
  where
  newPdTasks = pdTasks pd1 <> pdTasks pd2
  newPdResources = pdResources pd1 <> pdResources pd2
  newPdTasksPerResource = pdTasksPerResource pd1 <> pdTasksPerResource pd2
  newPdDependencies = pdDependencies pd1 <> pdDependencies pd2
  newPdRunningAssignmentGen = pdRunningAssignmentGen pd1


-- generators

-- Given a list of task ids , create dependencies

genDependencies :: Int -> [TaskId] -> Gen (M.Map TaskId [TaskId])
genDependencies maxDep tIds = do
  rootTIds <- sublistOf tIds
  deptIds <- shuffle (tIds L.\\ rootTIds)      -- dependent tasks, shuffled
  let m0  = M.fromList $ L.map (\tId -> (tId, [])) rootTIds
  foldM addOneTask m0 deptIds
  where
  addOneTask :: M.Map TaskId [TaskId] -> TaskId -> Gen (M.Map TaskId [TaskId])
  addOneTask m0 tId = do
    nbPreReq <- choose (1, M.size m0)
    let cappedNbPreReq = if nbPreReq > maxDep then maxDep else nbPreReq
    preReqTaskIds <- (shuffle $ M.keys m0) >>= (return . L.take cappedNbPreReq)

    return $ M.insert tId preReqTaskIds m0

{-
  generate nb tasks [nMin+1 .. nMin+n], each can be done by all resources
-}

genTasks :: String -> [Resource] -> Gen (Gen ManDays) -> Int -> Int -> Gen [Task]
genTasks projectId resources mandaysGenGen nMin nb = do
  let tIds = ["T-" ++ projectId ++ "-" ++ show (nMin+i) | i <- [1 .. nb]]
  mapM go tIds >>= shuffle
  where
  rIds = L.map rId resources
  go tId = do
    mandDaysGen <- mandaysGenGen
    return $ MkTask tId projectId mandDaysGen rIds


{-
Remove some of the resources that work on  task
-}

genRemoveResources :: [Task] -> Gen [Task]
genRemoveResources tasks = do
  mapM go tasks
  where
  go task@MkTask{..} = do
    resources <- sublistOf tCanBeDoneBy
    case L.null resources of
      True -> go task
      False -> return $ task {tCanBeDoneBy = resources}

