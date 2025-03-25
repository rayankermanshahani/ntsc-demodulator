module NTSC.Sync
  ( detectSynchronization,
    partitionScanLines,
  )
where

import DSP
import NTSC.Types

detectSynchronization :: FilteredSignal -> Double -> SyncedSignal
partitionScanLines :: SyncedSignal -> LinePartitionedSignal
