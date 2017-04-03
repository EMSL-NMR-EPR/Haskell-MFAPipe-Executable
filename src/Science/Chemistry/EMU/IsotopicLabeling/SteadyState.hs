{-# LANGUAGE FlexibleContexts #-}

module Science.Chemistry.EMU.IsotopicLabeling.SteadyState
( toSteadyStateEMU
, toFractionVectorDictEMU
) where

import qualified Control.Lens
import qualified Control.Parallel.Strategies
import           Data.Bifunctor (first)
import           Data.Graph.Inductive.Graph (OrdGr(..))
import qualified Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Graph.Extras.Node.Strict (NodeType(..))
import qualified Data.Graph.Inductive.Graph.Extras.Node.Strict
import           Data.Graph.Inductive.Graph.Instances ()
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.IntMap.Strict
import qualified Data.IntSet
import qualified Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Product(..), Sum(..))
import           Data.Proxy (Proxy(..))
import           Data.Set (Set)
import qualified Data.Set
import           Numeric.LinearAlgebra.HMatrix (Container(), Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Lens (AsMatrix(_Matrix), AsVector(), _Rows)
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.EMUGraph
import           Science.Chemistry.IsotopicLabeling.FractionVector
import           Science.Chemistry.IsotopicLabeling.SteadyState
import           Science.Chemistry.Types

toSteadyStateEMU
  :: (Ord i, Ord k, Container Vector e, Eq e, Fractional e)
  => Set i
  -> EMUGraph i k
  -> SteadyState i (EMU k) e
toSteadyStateEMU is0 = SteadyState . Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rseq (toSteadyStateLayerEMU (Data.Set.toAscList is0)) . Data.IntMap.Strict.elems . getEMUGraph
  where
    toSteadyStateLayerEMU :: (Ord i, Ord k, Container Vector e, Eq e, Fractional e) => [i] -> OrdGr Gr (EMU k) (Sum MixingProbability, i) -> SteadyStateLayer i (EMU k) e
    toSteadyStateLayerEMU is gr =
      let
        -- TODO Dulmage-Mendelsohn decomposition
        grs = [gr]
      in
        SteadyStateLayer (Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rseq (toSteadyStateStepEMU is) grs)
    toSteadyStateStepEMU :: (Ord i, Ord k, Container Vector e, Eq e, Fractional e) => [i] -> OrdGr Gr (EMU k) (Sum MixingProbability, i) -> SteadyStateStep i (EMU k) e
    toSteadyStateStepEMU is gr =
      let
        labNodesMap = Data.Graph.Inductive.Graph.Extras.Node.Strict.partitionLabNodes gr
        
        ~(isolatedNodes, isolatedNodeLabs) = unzip (Data.Map.Strict.findWithDefault [] Isolated labNodesMap)
        ~(sourceNodes, sourceNodeLabs) = unzip (Data.Map.Strict.findWithDefault [] Source labNodesMap)
        ~(sinkNodes, sinkNodeLabs) = unzip (Data.Map.Strict.findWithDefault [] Sink labNodesMap)
        ~(intermediateNodes, intermediateNodeLabs) = unzip (Data.Map.Strict.findWithDefault [] Intermediate labNodesMap)
        
        notSinkNodes = isolatedNodes ++ intermediateNodes ++ sourceNodes
        notSinkNodeLabs = isolatedNodeLabs ++ intermediateNodeLabs ++ sourceNodeLabs
        
        mkVectorMaybe iMap
          | Data.Map.Strict.null iMap = Nothing
          | otherwise =
              let
                vector = Numeric.LinearAlgebra.HMatrix.fromList (map (\i -> Data.Map.Strict.findWithDefault 0 i iMap) is)
              in
                if Numeric.LinearAlgebra.HMatrix.sumElements vector == 0
                  then Nothing
                  else Just vector
        
        notSinkAssocMatrix = mapMaybe go (pure (,) <*> (zip (enumFromThen 0 1) notSinkNodes) <*> (zip (enumFromThen 0 1) notSinkNodes))
          where
            go ~((rowIx, rowNode), (columnIx, columnNode))
              | rowNode == columnNode = (,) (rowIx, columnIx) <$> mkVectorMaybe (foldr (\ ~(_, _, (Sum (MixingProbability (Product p)), i)) -> Data.Map.Strict.insertWith (+) i (fromRational (negate p))) Data.Map.Strict.empty                                                        (Data.Graph.Inductive.Graph.out gr columnNode))
              | otherwise             = (,) (rowIx, columnIx) <$> mkVectorMaybe (foldr (\ ~(_, _, (Sum (MixingProbability (Product p)), i)) -> Data.Map.Strict.insertWith (+) i (fromRational         p))  Data.Map.Strict.empty (filter (\ ~(otherNode, _, _) -> rowNode == otherNode) (Data.Graph.Inductive.Graph.inn gr columnNode)))
        
        sinkAssocMatrix = mapMaybe go (pure (,) <*> (zip (enumFromThen 0 1) notSinkNodes) <*> (zip (enumFromThen 0 1) sinkNodes))
          where
            go ~((rowIx, rowNode), (columnIx, columnNode))
              | otherwise             = (,) (rowIx, columnIx) <$> mkVectorMaybe (foldr (\ ~(_, _, (Sum (MixingProbability (Product p)), i)) -> Data.Map.Strict.insertWith (+) i (fromRational (negate p))) Data.Map.Strict.empty (filter (\ ~(otherNode, _, _) -> rowNode == otherNode) (Data.Graph.Inductive.Graph.inn gr columnNode)))
        
        mkPartialDerivativeAssocMatrices assocs = Data.Map.Strict.fromAscList (filter (not . null . snd) (map (\ ~(vectorIx, i) -> (i, mapMaybe (\ ~(matrixIx, vector) -> let x = Numeric.LinearAlgebra.HMatrix.atIndex vector vectorIx in if x == 0 then Nothing else Just (matrixIx, x)) assocs)) (zip (enumFromThen 0 1) is)))
      in
        SteadyStateStep
          { _steadyStateStepU = is
          , _steadyStateStepX = notSinkNodeLabs
          , _steadyStateStepY = sinkNodeLabs
          , _steadyStateStepA = notSinkAssocMatrix
          , _steadyStateStepB = sinkAssocMatrix
          , _steadyStateStepA' = mkPartialDerivativeAssocMatrices notSinkAssocMatrix
          , _steadyStateStepB' = mkPartialDerivativeAssocMatrices sinkAssocMatrix
          }

toFractionVectorDictEMU
  :: (Ord i, Ord k, Container Vector e, Eq e, Num e, AsVector (FractionVector ty), Monoid (FractionVector ty e), Num (Vector e))
  => Proxy ty
  -> Int
  -> Set i
  -> Map k [(Rational, Matrix e)]
  -> FractionVectorDict ty i (EMU k) e
toFractionVectorDictEMU proxy0 base0 ixs0 m0 =
  let
    kMap = mkValues proxy0 base0 m0
    ikMap = mkSensitivities proxy0 ixs0 kMap
  in
    FractionVectorDict proxy0 kMap ikMap
  where
    mkValues
      :: (Ord k, Container Vector e, Eq e, Num e, AsVector (FractionVector ty), Monoid (FractionVector ty e), Num (Vector e))
      => Proxy ty
      -> Int
      -> Map k [(Rational, Matrix e)]
      -> Map (EMU k) (FractionVector ty e)
    mkValues proxy base = foldr (flip (foldr cata)) Data.Map.Strict.empty . map (uncurry scaleWithKey) . Data.Map.Strict.toAscList
      where
        cata = Data.Map.Strict.unionWith (curry (Control.Lens.view l))
          where
            l = Control.Lens.bimapping (_FractionVector proxy) (_FractionVector proxy) . Control.Lens.to (uncurry (+)) . Control.Lens.from (_FractionVector proxy)
        scaleWithKey k0 = map (uncurry ((. mkEMUs k0) . Data.Map.Strict.map . Control.Lens.over (_FractionVector proxy) . Numeric.LinearAlgebra.HMatrix.scale . fromRational))
          where
            mkEMUs k1 = Data.Map.Strict.fromList . map (mkEMU k1) . filter (not . null) . Data.List.subsequences . zip (enumFromThen 1 2) . Control.Lens.view (_Matrix . Control.Lens.from (Control.Lens.mapping (_FractionVector proxy) . _Rows))
              where
                mkEMU k2 = first (\xs -> EMU base [(k2, AtomIxSet xs)]) . foldMap (first Data.IntSet.singleton)
    mkSensitivities
      :: (Ord i, Ord k, Container Vector e, Eq e, Num e, AsVector (FractionVector ty))
      => Proxy ty
      -> Set i
      -> Map (EMU k) (FractionVector ty e)
      -> Map i (Map (EMU k) (FractionVector ty e))
    mkSensitivities proxy ixs kMap = Data.Set.foldr (\ix -> Data.Map.Strict.insert ix (Data.Map.Strict.map (Control.Lens.over (_FractionVector proxy) (Numeric.LinearAlgebra.HMatrix.konst 0 . Numeric.LinearAlgebra.HMatrix.size)) kMap)) Data.Map.Strict.empty ixs
