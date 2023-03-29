{- Data/Metrology/R.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   rae@cs.brynmawr.edu

   This file contains a definition of rational numbers at the type-level, in terms
   of a promoted datatype 'R'.
-}


{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, GADTs, PolyKinds, TemplateHaskell, ScopedTypeVariables, EmptyCase, CPP, FlexibleInstances, InstanceSigs, FlexibleContexts, StandaloneDeriving #-}


#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeApplications #-}
#endif
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.R
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

-- allow compilation even without Cabal
#ifndef MIN_VERSION_singletons
#define MIN_VERSION_singletons(a,b,c) 1
#endif






module Data.Metrology.R (
  -- * The 'R' datatype
  R(..),
#if MIN_VERSION_singletons(2,6,0)
  Sing, SZ(..), SR(..), STuple2(..),
#else
  Sing(..), SZ, SR, STuple2,
#endif

#if MIN_VERSION_singletons(1,0,0)
  -- ** Defunctionalization symbols (these can be ignored)
  ZeroSym0, SSym0, SSym1, PSym0, PSym1,
#endif


  -- * Conversions
  rToRational, srToRational

  ) where

import Data.Metrology.Z
import Data.Singletons.TH
#if MIN_VERSION_singletons(3,0,0)
import Data.Singletons.Base.TH 
#endif


-- | The datatype for type-level rational numbers.
-- A type-level rational number can be defined in terms of integers multiplied by 10^n
-- where n is the decimal position (negative after the decimal separator)
-- e.g. 215.15 = 2 * 10e2 + 1 * 10e1 + 5 * 10e0 + 1 * 10e(-1) + 5 * 10e(-2)
$(singletons [d| data R = C (Z, Z) R | T (Z, Z) deriving (Eq, Show)|])

-- | Convert a 'R' to a 'Rational'
rToRational :: R -> Rational
rToRational (T t) = zTupleToRational t
rToRational (C t r) = zTupleToRational t + rToRational r

-- floatToR :: Rational a => a -> R
-- floatToR 

zTupleToRational :: (Z, Z) -> Rational
zTupleToRational (x, e) = toRational (zToInt x) * 10 ^^ zToInt e

-- | Convert a singleton @R@ to a 'Rational'.
srToRational :: Sing (r :: R) -> Rational
srToRational = rToRational . fromSing

srCelsiusKelvinOffset = SC (STuple2 sTwo sTwo) -- 2e2
     $ SC (STuple2 sSeven sOne) -- 7e1
     $ SC (STuple2 sThree sZero) -- 3e0
     $ SC (STuple2 sOne sMOne) -- 1e(-1)
     $ ST (STuple2 sFive sMTwo) -- 5e(-2)

celsiusKelvinOffset
  = C (S (S Zero), S (S Zero)) -- 2e2
  $ C (S (S (S (S (S (S (S Zero)))))), S Zero) -- 7e1
  $ C (S (S (S Zero)), Zero) -- 3e0
  $ C (S Zero, P Zero) -- 1e(-1)
  $ T (S (S (S (S (S Zero)))), P (P Zero)) -- 5e(-2)

