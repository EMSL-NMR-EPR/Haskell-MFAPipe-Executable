module MFAPipe.Csv.Types.IsotopomerFractionVector
( IsotopomerFractionVectorRecords(..)
, IsotopomerFractionVectorRecord(..)
, encode
, encodeWith
, IndexedIsotopomerFractionVectorRecords(..)
, IndexedIsotopomerFractionVectorRecord(..)
, encodeIndexed
, encodeIndexedWith
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (ToField(), ToNamedRecord(toNamedRecord), EncodeOptions, Header)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Monoid.CartProduct (CartProduct(..))
import           Foreign.Storable (Storable())
import           MFAPipe.Csv.Constants
import qualified Numeric.LinearAlgebra.HMatrix
import           Science.Chemistry.EMU.EMU (EMU)
import qualified Science.Chemistry.EMU.HasSize.Class
import           Science.Chemistry.IsotopicLabeling.Isotopomer (Isotopomer)
import qualified Science.Chemistry.IsotopicLabeling.Isotopomer
import           Science.Chemistry.IsotopicLabeling.IsotopomerFractionVector (IsotopomerFractionVector(..))
import           Text.PrettyPrint.Leijen.Text (Pretty())

data IsotopomerFractionVectorRecords k e = IsotopomerFractionVectorRecords {-# UNPACK #-} !Int (Map (EMU k) (IsotopomerFractionVector e))
  deriving (Eq, Ord, Read, Show)

data IsotopomerFractionVectorRecord k e = IsotopomerFractionVectorRecord [Isotopomer] (EMU k) (IsotopomerFractionVector e)
  deriving (Eq, Ord, Read, Show)

instance (Pretty k, Storable e, ToField k, ToField e) => ToNamedRecord (IsotopomerFractionVectorRecord k e) where
  toNamedRecord (IsotopomerFractionVectorRecord isotopomers emu xs) = Data.Csv.namedRecord $
      Data.Csv.namedField cEMUFieldName emu
    : zipWith Data.Csv.namedField (map Data.Csv.toField isotopomers) (Numeric.LinearAlgebra.HMatrix.toList (getCartProduct (getIsotopomerFractionVector xs)))

encode :: (Pretty k, Storable e, ToField k, ToField e) => IsotopomerFractionVectorRecords k e -> IntMap ByteString
encode  = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (Pretty k, Storable e, ToField k, ToField e) => EncodeOptions -> IsotopomerFractionVectorRecords k e -> IntMap ByteString
encodeWith opts (IsotopomerFractionVectorRecords base m) = Data.IntMap.Strict.mapWithKey (\n -> let isotopomers = Science.Chemistry.IsotopicLabeling.Isotopomer.isotopomers base n in Data.Csv.encodeByNameWith opts (toHeader isotopomers) . map (uncurry (IsotopomerFractionVectorRecord isotopomers))) (Science.Chemistry.EMU.HasSize.Class.partitionBySizeWith fst (Data.Map.Strict.toAscList m))

toHeader :: [Isotopomer] -> Header
toHeader isotopomers = Data.Csv.header $
    cEMUFieldName
  : map Data.Csv.toField isotopomers

data IndexedIsotopomerFractionVectorRecords i k e = IndexedIsotopomerFractionVectorRecords {-# UNPACK #-} !Int (Map i (Map (EMU k) (IsotopomerFractionVector e)))
  deriving (Eq, Ord, Read, Show)

data IndexedIsotopomerFractionVectorRecord i k e = IndexedIsotopomerFractionVectorRecord [Isotopomer] i (EMU k) (IsotopomerFractionVector e)
  deriving (Eq, Ord, Read, Show)

instance (Pretty k, Storable e, ToField i, ToField k, ToField e) => ToNamedRecord (IndexedIsotopomerFractionVectorRecord i k e) where
  toNamedRecord (IndexedIsotopomerFractionVectorRecord isotopomers ix emu xs) = Data.Csv.namedRecord $
      Data.Csv.namedField cFluxVarFieldName ix
    : Data.Csv.namedField cEMUFieldName emu
    : zipWith Data.Csv.namedField (map Data.Csv.toField isotopomers) (Numeric.LinearAlgebra.HMatrix.toList (getCartProduct (getIsotopomerFractionVector xs)))

encodeIndexed :: (Pretty k, Storable e, ToField i, ToField k, ToField e) => IndexedIsotopomerFractionVectorRecords i k e -> IntMap ByteString
encodeIndexed = encodeIndexedWith Data.Csv.defaultEncodeOptions

encodeIndexedWith :: (Pretty k, Storable e, ToField i, ToField k, ToField e) => EncodeOptions -> IndexedIsotopomerFractionVectorRecords i k e -> IntMap ByteString
encodeIndexedWith opts (IndexedIsotopomerFractionVectorRecords base m) = Data.IntMap.Strict.mapWithKey (\n -> let isotopomers = Science.Chemistry.IsotopicLabeling.Isotopomer.isotopomers base n in Data.Csv.encodeByNameWith opts (toHeaderIndexed isotopomers) . map (uncurry (uncurry . IndexedIsotopomerFractionVectorRecord isotopomers))) (Science.Chemistry.EMU.HasSize.Class.partitionBySizeWith (fst . snd) (concatMap (uncurry (map . (,))) (Data.Map.Strict.toAscList (Data.Map.Strict.map Data.Map.Strict.toAscList m))))

toHeaderIndexed :: [Isotopomer] -> Header
toHeaderIndexed isotopomers = Data.Csv.header $
    cFluxVarFieldName
  : cEMUFieldName
  : map Data.Csv.toField isotopomers
