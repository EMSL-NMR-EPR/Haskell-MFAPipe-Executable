{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Science.Chemistry.EMU.IsotopicLabeling.DSL.Evaluate.Class
( ExprF, Expr
, EMUExprF , EMUExpr
, runSteadyStateEMUExpr
, runSteadyStateEMUExpr'
, fractionTypeDictEMU
, typedEMUs
) where

import           Control.Applicative (Const(..))
import           Control.Comonad.Trans.Rall (runRallT)
import           Control.Monad.Parallel (MonadParallel())
import           Control.Monad.Trans.Except (ExceptT(..), withExceptT)
import           Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import           Data.Bifunctor (bimap)
import qualified Data.Foldable
import           Data.Functor.Product (Product(..))
import           Data.Functor.Sum (Sum(..))
import           Data.Graph.Inductive.Query.DFS.EdgeSmoothing (Smoothable())
import           Data.Injective (Fix, FloatingF(..), FractionalF(..), NumF(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Monoid.EndoM (EndoM(..))
import           Data.Proxy (Proxy(..))
import           Data.Set (Set)
import qualified Data.Set
import           Numeric.LinearAlgebra.HMatrix (Container(), Field(), Numeric(), IndexOf, Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.EMUGraph
import           Science.Chemistry.EMU.Factorized.Class
import           Science.Chemistry.EMU.IsotopicLabeling.SteadyState
import           Science.Chemistry.EMU.Optimized.Class
import           Science.Chemistry.IsotopicLabeling.DSL.FromDict.Class
import           Science.Chemistry.IsotopicLabeling.FractionType
import           Science.Chemistry.IsotopicLabeling.FractionTypeDict
import           Science.Chemistry.IsotopicLabeling.FractionVector
import           Science.Chemistry.IsotopicLabeling.SteadyState

class (Ord k, FromDict i e f) => BuildSteadyStateEMU i k e f where
  buildSteadyStateEMU
    :: (MonadParallel m)
    => Proxy f
    -> Int
    -> Set i
    -> EMUGraph i k
    -> Map k [(Rational, Matrix e)]
    -> Maybe (Map FractionType (Set (EMU k)))
    -> ReaderT (Vector e) (ExceptT (DictError f i) m) (Dict f i e)

instance (BuildSteadyStateEMU i k e f, BuildSteadyStateEMU i k e g) => BuildSteadyStateEMU i k e (f `Sum` g) where
  buildSteadyStateEMU (Proxy :: Proxy (f `Sum` g)) base ixs gr kMap tyMapMaybe = pure Pair <*> mapReaderT (withExceptT Left) (buildSteadyStateEMU (Proxy :: Proxy f) base ixs gr kMap tyMapMaybe) <*> mapReaderT (withExceptT Right) (buildSteadyStateEMU (Proxy :: Proxy g) base ixs gr kMap tyMapMaybe)

instance (Ord i, Ord k, Num e) => BuildSteadyStateEMU i k e NumF where
  buildSteadyStateEMU Proxy _ _ _ _ _ = return (Const ())

instance (Ord i, Ord k, Fractional e) => BuildSteadyStateEMU i k e FractionalF where
  buildSteadyStateEMU Proxy _ _ _ _ _ = return (Const ())

instance (Ord i, Ord k, Floating e) => BuildSteadyStateEMU i k e FloatingF where
  buildSteadyStateEMU Proxy _ _ _ _ _ = return (Const ())

instance (Ord i, Smoothable i, Ord k, Container Vector e, Eq e, Field e, Num (Vector e), Numeric e) => BuildSteadyStateEMU i k e (LookupFractionVectorF 'IsotopomerFractionTy (EMU k)) where
  buildSteadyStateEMU Proxy base ixs gr kMap tyMapMaybe = do
    case tyMapMaybe of
      Nothing -> do
        let
          t = toSteadyStateEMU ixs gr
          dict = toFractionVectorDictEMU Proxy base ixs kMap
        appEndoM (runSteadyState t) dict
      Just tyMap -> do
        case Data.Map.Strict.lookup IsotopomerFractionTy tyMap of
          Nothing -> do
            return (emptyFractionVectorDict Proxy)
          Just ks
            | Data.Set.null ks -> do
                return (emptyFractionVectorDict Proxy)
            | otherwise -> do
                let
                  t = toSteadyStateEMU ixs (optimize ks gr)
                  dict = toFractionVectorDictEMU Proxy base ixs kMap
                appEndoM (runSteadyState t) dict

instance (Ord i, Smoothable i, Ord k, Container Vector e, Eq e, Field e, Num (Vector e), Numeric e) => BuildSteadyStateEMU i k e (LookupFractionVectorF 'MassFractionTy (EMU k)) where
  buildSteadyStateEMU Proxy base ixs gr kMap tyMapMaybe = do
    case tyMapMaybe of
      Nothing -> do
        let
          t = toSteadyStateEMU ixs gr
          dict = toFractionVectorDictEMU Proxy base ixs kMap
        appEndoM (runSteadyState t) dict
      Just tyMap -> do
        case Data.Map.Strict.lookup MassFractionTy tyMap of
          Nothing -> do
            return (emptyFractionVectorDict Proxy)
          Just ks
            | Data.Set.null ks -> do
                return (emptyFractionVectorDict Proxy)
            | otherwise -> do
                let
                  t = toSteadyStateEMU ixs (optimize ks gr)
                  dict = toFractionVectorDictEMU Proxy base ixs kMap
                appEndoM (runSteadyState t) dict

type ExprF k = LookupFractionVectorF 'IsotopomerFractionTy k `Sum` (LookupFractionVectorF 'MassFractionTy k `Sum` (NumF `Sum` (FractionalF `Sum` FloatingF)))

type Expr k = Fix (ExprF k)

type EMUExprF k = ExprF (EMU k)

type EMUExpr k = Expr (EMU k)

evaluateSteadyStateEMU
  :: (MonadParallel m, BuildSteadyStateEMU i k e f, Traversable t, Container Vector e)
  => Proxy f
  -> (t (Fix f) -> Maybe (Map FractionType (Set (EMU k))))
  -> Int
  -> Set i
  -> EMUGraph i k
  -> Map k [(Rational, Matrix e)]
  -> t (Fix f)
  -> ReaderT (Vector e) (ExceptT (DictError f i) m) (Maybe (Map FractionType (Set (EMU k))), Dict f i e, ExceptT (DictError f i) m (Vector e, Matrix e))
evaluateSteadyStateEMU proxy mkTyMapMaybe base ixs gr kMap fs = do
  let
    tyMapMaybe = mkTyMapMaybe fs
  dict <- buildSteadyStateEMU proxy base ixs gr kMap tyMapMaybe
  return (tyMapMaybe, dict, evaluateJacobian proxy ixs dict fs)

evaluateJacobian
  :: (Monad m, FromDict i e f, Traversable t, Container Vector e)
  => Proxy f
  -> Set i
  -> Dict f i e
  -> t (Fix f)
  -> ExceptT (DictError f i) m (Vector e, Matrix e)
evaluateJacobian Proxy ixs dict = fmap (bimap Numeric.LinearAlgebra.HMatrix.fromList (Numeric.LinearAlgebra.HMatrix.fromLists . map (\f -> map f (Data.Set.toAscList ixs))) . unzip . map runRallT . Data.Foldable.toList) . mapM (fromDict dict)

fractionTypeDictEMU
  :: (Foldable f, Ord k)
  => f (EMUExpr k)
  -> FractionTypeDict (EMU k) (IndexOf Vector)
fractionTypeDictEMU = foldMap fractionTypeDict

typedEMUs
  :: (Foldable f, Ord k)
  => f (EMUExpr k)
  -> Map FractionType (Set (EMU k))
typedEMUs = foldrWithKey' (\ty k _a _n -> Data.Map.Strict.alter (Just . Data.Set.union (Data.Set.fromList (factors k)) . Data.Set.insert k . maybe Data.Set.empty id) ty) Data.Map.Strict.empty . fractionTypeDictEMU

runSteadyStateEMUExpr
  :: (MonadParallel m, Traversable t, Ord i, Smoothable i, Ord k, Eq e, Field e, Num (Vector e), Numeric e)
  => Int
  -> Set i
  -> EMUGraph i k
  -> [(Map k [(Rational, Matrix e)], t (EMUExpr k))]
  -> ReaderT (Vector e) (ExceptT (DictError (EMUExprF k) i) m) [(Maybe (Map FractionType (Set (EMU k))), Dict (EMUExprF k) i e, ExceptT (DictError (EMUExprF k) i) m (Vector e, Matrix e))]
runSteadyStateEMUExpr base ixs gr = mapM (uncurry (evaluateSteadyStateEMU Proxy (Just . typedEMUs) base ixs gr))

runSteadyStateEMUExpr'
  :: (MonadParallel m, Ord i, Smoothable i, Ord k, Eq e, Field e, Num (Vector e), Numeric e)
  => Int
  -> Set i
  -> EMUGraph i k
  -> [Map k [(Rational, Matrix e)]]
  -> ReaderT (Vector e) (ExceptT (DictError (EMUExprF k) i) m) [Dict (EMUExprF k) i e]
runSteadyStateEMUExpr' base ixs gr = mapM (\kMap -> (\ ~(_, dict, _) -> dict) <$> evaluateSteadyStateEMU (Proxy :: Proxy (EMUExprF k)) (\(Const ()) -> Nothing) base ixs gr kMap (Const ()))
