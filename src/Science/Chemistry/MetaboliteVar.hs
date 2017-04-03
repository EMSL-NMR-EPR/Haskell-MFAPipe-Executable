-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.MetaboliteVar
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of metabolite variables.
-----------------------------------------------------------------------------

module Science.Chemistry.MetaboliteVar
( -- * Metabolite Variables
  MetaboliteVar(..)
  -- * Predicates
, isIntracellular , isExtracellular
) where

import           Data.Csv (ToField(toField))
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import qualified Text.PrettyPrint.Leijen.Text

-- | A metabolite variable.
data MetaboliteVar a
  = Intracellular a
  | Extracellular a
  deriving (Eq, Ord, Read, Show)

instance Functor MetaboliteVar where
  fmap f (Intracellular x) = Intracellular (f x)
  fmap f (Extracellular x) = Extracellular (f x)
  {-# INLINE fmap #-}

instance (Pretty a) => Pretty (MetaboliteVar a) where
  pretty (Intracellular x) = pretty x
  pretty (Extracellular x) = pretty x

instance (Pretty a) => ToField (MetaboliteVar a) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | Return 'True' if the given metabolite variable is 'Intracellular', 'False' otherwise.
isIntracellular :: MetaboliteVar a -> Bool
isIntracellular (Intracellular _) = True
isIntracellular _ = False
{-# INLINE isIntracellular #-}

-- | Return 'True' if the given metabolite variable is 'Extracellular', 'False' otherwise.
isExtracellular :: MetaboliteVar a -> Bool
isExtracellular (Extracellular _) = True
isExtracellular _ = False
{-# INLINE isExtracellular #-}
