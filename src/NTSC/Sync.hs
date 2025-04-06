{- src/NTSC/Sync.hs -}
module NTSC.Sync
  ( detectSynchronization,
    partitionScanLines,
  )
where

import DSP
import NTSC.Types

detectSynchronization :: FilteredSignal -> Double -> SyncedSignal
detectSynchronization = error "TODO"

partitionScanLines :: SyncedSignal -> LinePartitionedSignal
partitionScanLines = error "TODO"
