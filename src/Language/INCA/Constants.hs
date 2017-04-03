-----------------------------------------------------------------------------
-- |
-- Module      :  Language.INCA.Constants
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports constants for the representation of INCA syntax.
-----------------------------------------------------------------------------

module Language.INCA.Constants
( -- * Constants
  -- ** Metabolic flux variables
  cINCAFluxVarSeparator
, cINCAFluxVarExchange , cINCAFluxVarTransport
, cINCAFluxVarDirectionForwards , cINCAFluxVarDirectionBackwards
  -- ** Metabolite variables
, cINCAMetaboliteVarSeparator
, cINCAMetaboliteVarCompartmentNameExtracellular
) where

-- | Separator for the name and direction of a metabolic flux variable in INCA syntax.
cINCAFluxVarSeparator :: Char
cINCAFluxVarSeparator = '.'
{-# INLINE cINCAFluxVarSeparator #-}

-- | Prefix for metabolic flux variables in INCA syntax that denote exchange chemical reactions.
cINCAFluxVarExchange :: Char
cINCAFluxVarExchange = 'v'
{-# INLINE cINCAFluxVarExchange #-}

-- | Prefix for metabolic flux variables in INCA syntax that denote transport chemical reactions.
cINCAFluxVarTransport :: Char
cINCAFluxVarTransport = 'b'
{-# INLINE cINCAFluxVarTransport #-}

-- | Suffix for metabolic flux variables in INCA syntax that denote forwards chemical reactions.
cINCAFluxVarDirectionForwards :: Char
cINCAFluxVarDirectionForwards = 'f'
{-# INLINE cINCAFluxVarDirectionForwards #-}

-- | Suffix for metabolic flux variables in INCA syntax that denote backwards chemical reactions.
cINCAFluxVarDirectionBackwards :: Char
cINCAFluxVarDirectionBackwards = 'b'
{-# INLINE cINCAFluxVarDirectionBackwards #-}

-- | Separator for the name and compartment name of a metabolite variable in INCA syntax.
cINCAMetaboliteVarSeparator :: Char
cINCAMetaboliteVarSeparator = '.'
{-# INLINE cINCAMetaboliteVarSeparator #-}

-- | Suffix for metabolite variables in INCA syntax that denote extracellular metabolites.
cINCAMetaboliteVarCompartmentNameExtracellular :: String
cINCAMetaboliteVarCompartmentNameExtracellular = "ext"
{-# INLINE cINCAMetaboliteVarCompartmentNameExtracellular #-}
