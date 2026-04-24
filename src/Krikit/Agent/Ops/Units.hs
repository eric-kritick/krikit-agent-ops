{-# LANGUAGE DerivingStrategies #-}

-- | Unit-tagged numeric types.
--
-- Wrapping @Int@ in 'Seconds' / 'Milliseconds' keeps dimensional mistakes
-- (passing 30 milliseconds where 30 seconds was expected) at type-error
-- time rather than "why did my smoke test time out after 30ms".
--
-- Conversions are explicit functions; no 'Num' instance, because arithmetic
-- between units (Seconds + Milliseconds) is dimensionally wrong.
module Krikit.Agent.Ops.Units
    ( -- * Seconds
      Seconds (..)
    , secondsInt
    , secondsToMicros

      -- * Milliseconds
    , Milliseconds (..)
    , millisInt
    , secondsToMillis
    ) where

-- | Duration in whole seconds. Use for timeouts + anything configured in
-- seconds-granularity by the operator.
newtype Seconds = Seconds Int
    deriving stock   (Eq, Show)
    deriving newtype (Ord)

secondsInt :: Seconds -> Int
secondsInt (Seconds n) = n

secondsToMicros :: Seconds -> Int
secondsToMicros (Seconds n) = n * 1_000_000
{-# INLINE secondsToMicros #-}

-- | Duration in whole milliseconds. Use for elapsed-time bookkeeping
-- (subprocess wall-clock, tier elapsed) — the natural granularity the
-- system clock provides.
newtype Milliseconds = Milliseconds Int
    deriving stock   (Eq, Show)
    deriving newtype (Ord)

millisInt :: Milliseconds -> Int
millisInt (Milliseconds n) = n

secondsToMillis :: Seconds -> Milliseconds
secondsToMillis (Seconds n) = Milliseconds (n * 1000)
{-# INLINE secondsToMillis #-}
