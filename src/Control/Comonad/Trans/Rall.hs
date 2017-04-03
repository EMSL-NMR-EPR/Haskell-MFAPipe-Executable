{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Rall
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- The automatic differentiation (AD) comonad holds a value along with its
-- derivative.
--
-- This module specifies the AD comonad transformer (a.k.a., Rall), which is
-- isomorphic to the coreader comonad, where the environment and value types
-- are equal.
--
-- See also
--
--  * \"The arithmetic of differentiation\",
--    by L. B. Rall,
--    /Mathematics Magazine/ 59:5 (1986) 275-282, online at
--    <http://www.jstor.org/stable/2689402>.
--
-----------------------------------------------------------------------------

module Control.Comonad.Trans.Rall
( -- * The AD comonad
  Rall , rall , runRall
  -- * The AD comonad transformer
, RallT(..) , runRallT
, lowerRallT
  -- * as a coreader comonad transformer
, rallToEnv , envToRall
) where

import Control.Applicative (liftA, liftA2)
import Control.Comonad
import Control.Comonad.Env.Class
import Control.Comonad.Hoist.Class
import Control.Comonad.Store.Class
import Control.Comonad.Traced.Class
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env (EnvT(..))
import Data.Functor.Identity (Identity(..))

type Rall = RallT Identity

-- | Create a 'Rall' using a value and its derivative.
rall :: a -> a -> Rall a
rall a a' = RallT a (Identity a')
{-# INLINE rall #-}

runRall :: Rall a -> (a, a)
runRall (RallT a (Identity a')) = (a, a')
{-# INLINE runRall #-}

data RallT w a = RallT a (w a)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

runRallT :: RallT w a -> (a, w a)
runRallT (RallT a w) = (a, w)
{-# INLINE runRallT #-}

-- | Returns the derivative. This differs from 'extract' in that it will not continue extracting the value from the contained comonad.
lowerRallT :: RallT w a -> w a
lowerRallT (RallT _ w) = w
{-# INLINE lowerRallT #-}

instance (Comonad w) => Comonad (RallT w) where
  duplicate x0@(RallT a w) = RallT x0 (extend (RallT a) w)
  {-# INLINE duplicate #-}
  extract (RallT _ w) = extract w
  {-# INLINE extract #-}

instance (ComonadApply w) => ComonadApply (RallT w) where
  (RallT f wf) <@> (RallT x wx) = RallT (f x) (wf <@> wx)
  {-# INLINE (<@>) #-}

instance ComonadTrans RallT where
  lower (RallT _ w) = w
  {-# INLINE lower #-}

instance ComonadHoist RallT where
  cohoist f (RallT a w) = RallT a (f w)
  {-# INLINE cohoist #-}

instance (ComonadEnv e w) => ComonadEnv e (RallT w) where
  ask = ask . lower
  {-# INLINE ask #-}

instance (ComonadStore s w) => ComonadStore s (RallT w) where
  pos = pos . lower
  {-# INLINE pos #-}
  peek s = peek s . lower
  {-# INLINE peek #-}
  experiment f = experiment f . lower
  {-# INLINE experiment #-}

instance (ComonadTraced m w) => ComonadTraced m (RallT w) where
  trace m = trace m . lower
  {-# INLINE trace #-}

instance (Applicative m) => Applicative (RallT m) where
  pure x = RallT x (pure x)
  {-# INLINE pure #-}
  (RallT f mf) <*> (RallT x mx) = RallT (f x) (mf <*> mx)
  {-# INLINE (<*>) #-}

-- | Sum and product rules.
instance (Applicative m, Num a) => Num (RallT m a) where
  (RallT xL mL) + (RallT xR mR) = RallT (xL + xR) (liftA2 (+) mL mR)
  {-# INLINE (+) #-}
  (RallT xL mL) - (RallT xR mR) = RallT (xL - xR) (liftA2 (-) mL mR)
  {-# INLINE (-) #-}
  (RallT xL mL) * (RallT xR mR) = RallT (xL * xR) (liftA2 (\dxL dxR -> (dxL * xR) + (xL * dxR)) mL mR)
  {-# INLINE (*) #-}
  negate (RallT x m) = RallT (negate x) (liftA negate m)
  {-# INLINE negate #-}
  abs (RallT x m) = RallT (abs x) (liftA signum m)
  {-# INLINE abs #-}
  signum (RallT x m) = RallT (signum x) (liftA abs m)
  {-# INLINE signum #-}
  fromInteger x = RallT (fromInteger x) (pure 0)
  {-# INLINE fromInteger #-}

-- | Quotient rule.
instance (Applicative m, Fractional a) => Fractional (RallT m a) where
  (RallT xL mL) / (RallT xR mR) = RallT (xL / xR) (liftA2 (\dxL dxR -> ((dxL * xR) - (xL * dxR)) / (xR * xR)) mL mR)
  {-# INLINE (/) #-}
  recip (RallT x m) = RallT (recip x) (liftA (\dx -> (negate dx) / (x * x)) m)
  {-# INLINE recip #-}
  fromRational x = RallT (fromRational x) (pure 0)
  {-# INLINE fromRational #-}

-- | Derivatives of exponential, logarithmic and trigonometric functions (using the chain rule).
instance (Applicative m, Floating a) => Floating (RallT m a) where
  pi = RallT pi (pure 0)
  {-# INLINE pi #-}
  exp (RallT x m) = RallT (exp x) (liftA (* (exp x)) m)
  {-# INLINE exp #-}
  log (RallT x m) = RallT (log x) (liftA (* (recip x)) m)
  {-# INLINE log #-}
  sin (RallT x m) = RallT (sin x) (liftA (* (cos x)) m)
  {-# INLINE sin #-}
  cos (RallT x m) = RallT (cos x) (liftA (* (negate (sin x))) m)
  {-# INLINE cos #-}
  asin (RallT x m) = RallT (asin x) (liftA (* (recip (sqrt (1 - (x * x))))) m)
  {-# INLINE asin #-}
  acos (RallT x m) = RallT (acos x) (liftA (* (negate (recip (sqrt (1 - (x * x)))))) m)
  {-# INLINE acos #-}
  atan (RallT x m) = RallT (atan x) (liftA (* (recip (1 + (x * x)))) m)
  {-# INLINE atan #-}
  sinh (RallT x m) = RallT (sinh x) (liftA (* (cosh x)) m)
  {-# INLINE sinh #-}
  cosh (RallT x m) = RallT (cosh x) (liftA (* (sinh x)) m)
  {-# INLINE cosh #-}
  asinh (RallT x m) = RallT (asinh x) (liftA (* (recip (sqrt ((x * x) + 1)))) m)
  {-# INLINE asinh #-}
  acosh (RallT x m) = RallT (acosh x) (liftA (* (recip (sqrt ((x * x) - 1)))) m)
  {-# INLINE acosh #-}
  atanh (RallT x m) = RallT (atanh x) (liftA (* (recip (1 - (x * x)))) m)
  {-# INLINE atanh #-}

rallToEnv :: RallT w a -> EnvT a w a
rallToEnv (RallT a w) = EnvT a w
{-# INLINE rallToEnv #-}

envToRall :: EnvT a w a -> RallT w a
envToRall (EnvT a w) = RallT a w
{-# INLINE envToRall #-}
