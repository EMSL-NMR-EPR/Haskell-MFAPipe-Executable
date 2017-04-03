-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE StandaloneDeriving #-}
--
-- module MFAPipe.Csv.Types.NullspaceMatrix
-- ( NullspaceMatrixRecords(..)
-- , NullspaceMatrixRecord(..)
-- , encode
-- , encodeWith
-- ) where
--
-- import           Control.Applicative (liftA2)
-- import           Data.Bifunctor (first)
-- import           Data.ByteString.Lazy (ByteString)
-- import qualified Data.Csv
-- import           Data.Csv (ToField(), ToNamedRecord(toNamedRecord), EncodeOptions, Header)
-- import qualified Data.Map.Strict
-- import           Data.Maybe (isJust)
-- import           Data.Set (Set)
-- import qualified Data.Set
-- import           MFAPipe.Csv.Constants
-- import           Numeric.LinearAlgebra.HMatrix (Container(), Element(), Vector)
-- import qualified Numeric.LinearAlgebra.HMatrix
-- import           Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan (Nullspace(..))
-- import           Science.Chemistry.Types
-- import           Text.PrettyPrint.Leijen.Text (Pretty())
--
-- data NullspaceMatrixRecords i e = NullspaceMatrixRecords (Set i) (Nullspace i e)
--   deriving (Read, Show)
--
-- deriving instance (Eq i, Eq e, Container Vector e, Num e) => Eq (NullspaceMatrixRecords i e)
--
-- data NullspaceMatrixRecord i e = NullspaceMatrixRecord i [(Set i, e)]
--   deriving (Eq, Ord, Read, Show)
--
-- instance (ToField i, ToField e, Pretty i) => ToNamedRecord (NullspaceMatrixRecord i e) where
--   toNamedRecord (NullspaceMatrixRecord ix xs) = Data.Csv.namedRecord $
--       Data.Csv.namedField cFluxVarFieldName ix
--     : map (uncurry Data.Csv.namedField . first (Data.Csv.toField . AtomKeyList . Data.Set.toAscList)) xs
--
-- encode :: (ToField i, ToField e, Ord i, Pretty i, Element e) => NullspaceMatrixRecords i e -> ByteString
-- encode = encodeWith Data.Csv.defaultEncodeOptions
--
-- encodeWith :: (ToField i, ToField e, Ord i, Pretty i, Element e) => EncodeOptions -> NullspaceMatrixRecords i e -> ByteString
-- encodeWith opts = liftA2 (Data.Csv.encodeByNameWith opts) toHeader toList
--
-- toHeader :: (Pretty i) => NullspaceMatrixRecords i e -> Header
-- toHeader (NullspaceMatrixRecords _ (Nullspace _ _ _ _ bwd)) = Data.Csv.header $
--     cFluxVarFieldName
--   -- : map Data.Csv.toField (catMaybes (Data.Map.Strict.keys bwd))
--   : map (Data.Csv.toField . AtomKeyList . Data.Set.toAscList . snd) (filter (isJust . fst) (Data.Map.Strict.toAscList bwd))
--
-- toList :: (Element e) => NullspaceMatrixRecords i e -> [NullspaceMatrixRecord i e]
-- toList (NullspaceMatrixRecords ixs (Nullspace _ rcef_ker _ _ bwd)) = zipWith NullspaceMatrixRecord (Data.Set.toAscList ixs) (map (zip (map snd (filter (isJust . fst) (Data.Map.Strict.toAscList bwd)))) (Numeric.LinearAlgebra.HMatrix.toLists rcef_ker))

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module MFAPipe.Csv.Types.NullspaceMatrix
( NullspaceMatrixRecords(..)
, NullspaceMatrixRecord(..)
, encode
, encodeWith
) where

import           Control.Applicative (liftA2)
import           Data.Bifunctor (bimap)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (ToField(), ToNamedRecord(toNamedRecord), EncodeOptions, Field, Header)
import qualified Data.Map.Strict
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set
import           MFAPipe.Csv.Constants
import           Numeric.LinearAlgebra.HMatrix (Container(), Element(), IndexOf, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan (Nullspace(..))

data NullspaceMatrixRecords i e = NullspaceMatrixRecords (Set i) (Nullspace i e)
  deriving (Read, Show)

deriving instance (Eq i, Eq e, Container Vector e, Num e) => Eq (NullspaceMatrixRecords i e)

data NullspaceMatrixRecord i e = NullspaceMatrixRecord i (Maybe (IndexOf Vector)) [(IndexOf Vector, e)]
  deriving (Eq, Ord, Read, Show)

instance (ToField i, ToField e) => ToNamedRecord (NullspaceMatrixRecord i e) where
  toNamedRecord (NullspaceMatrixRecord ix vectorIxMaybe xs) = Data.Csv.namedRecord $
      Data.Csv.namedField cFluxVarFieldName ix
    : Data.Csv.namedField cFreeVarFieldName (maybe (Data.Csv.toField "") toFieldFreeVar vectorIxMaybe)
    : map (bimap toFieldFreeVar Data.Csv.toField) xs

encode :: (ToField i, ToField e, Ord i, Element e) => NullspaceMatrixRecords i e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField i, ToField e, Ord i, Element e) => EncodeOptions -> NullspaceMatrixRecords i e -> ByteString
encodeWith opts = liftA2 (Data.Csv.encodeByNameWith opts) toHeader toList

toHeader :: (ToField i) => NullspaceMatrixRecords i e -> Header
toHeader (NullspaceMatrixRecords _ (Nullspace _ _ _ _ bwd)) = Data.Csv.header $
    cFluxVarFieldName
  : cFreeVarFieldName
  : map Data.Csv.toField (map toFieldFreeVar (catMaybes (Data.Map.Strict.keys bwd)))

toList :: (Ord i, Element e) => NullspaceMatrixRecords i e -> [NullspaceMatrixRecord i e]
toList (NullspaceMatrixRecords ixs (Nullspace _ rcef_ker _ fwd bwd)) = zipWith (\ix xs -> NullspaceMatrixRecord ix (Data.Map.Strict.findWithDefault Nothing ix fwd) xs) (Data.Set.toAscList ixs) (map (zip (catMaybes (Data.Map.Strict.keys bwd))) (Numeric.LinearAlgebra.HMatrix.toLists rcef_ker))

toFieldFreeVar :: IndexOf Vector -> Field
toFieldFreeVar = Data.Csv.toField . (:) 'u' . show . succ
