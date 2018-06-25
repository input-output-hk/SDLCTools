
{-# LANGUAGE RecordWildCards #-}

module Planning.Processing

where




import            Gtsim.Types


getFinalEndTime :: SimState -> Day
getFinalEndTime MkSimState{..} = (caEndTime . head) sstCompletedAssignments
