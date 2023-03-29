{- Data/Metrology.Offset.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   rae@cs.brynmawr.edu

   This file defines the Offset kind.

   Offsets represent units with a shifted zero-point, and the same scale.
   E.g. °C = K + 273.15 K, with (a + b) °C = (a + b) K
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, CPP #-}

-- allow compilation even without Cabal
#ifndef MIN_VERSION_singletons
#define MIN_VERSION_singletons(a,b,c) 1
#endif

#if __GLASGOW_HASKELL__ >= 900
{-# OPTIONS_GHC -Wno-star-is-type #-}
#endif

module Data.Metrology.Offset where

import Data.Metrology.R as R

-- | This will only be used at the kind level. It holds a unit
-- with its offset.
data Offset star = O star R
