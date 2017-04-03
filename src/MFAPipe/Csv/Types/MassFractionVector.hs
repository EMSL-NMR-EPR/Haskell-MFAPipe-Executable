module MFAPipe.Csv.Types.MassFractionVector
( MassFractionVectorRecords(..)
, MassFractionVectorRecord(..)
, encode
, encodeWith
, IndexedMassFractionVectorRecords(..)
, IndexedMassFractionVectorRecord(..)
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
import           Data.Monoid.Conv (Conv(..))
import           Foreign.Storable (Storable())
import           MFAPipe.Csv.Constants
import qualified Numeric.LinearAlgebra.HMatrix
import           Science.Chemistry.EMU.EMU (EMU)
import qualified Science.Chemistry.EMU.HasSize.Class
import           Science.Chemistry.IsotopicLabeling.MassFraction (MassFraction)
import qualified Science.Chemistry.IsotopicLabeling.MassFraction
import           Science.Chemistry.IsotopicLabeling.MassFractionVector (MassFractionVector(..))
import           Text.PrettyPrint.Leijen.Text (Pretty())

data MassFractionVectorRecords k e = MassFractionVectorRecords {-# UNPACK #-} !Int (Map (EMU k) (MassFractionVector e))
  deriving (Eq, Ord, Read, Show)

data MassFractionVectorRecord k e = MassFractionVectorRecord [MassFraction] (EMU k) (MassFractionVector e)
  deriving (Eq, Ord, Read, Show)

instance (Pretty k, Storable e, ToField k, ToField e) => ToNamedRecord (MassFractionVectorRecord k e) where
  toNamedRecord (MassFractionVectorRecord massFractions emu xs) = Data.Csv.namedRecord $
      Data.Csv.namedField cEMUFieldName emu
    : zipWith Data.Csv.namedField (map Data.Csv.toField massFractions) (Numeric.LinearAlgebra.HMatrix.toList (getConv (getMassFractionVector xs)))

encode :: (Pretty k, Storable e, ToField k, ToField e) => MassFractionVectorRecords k e -> IntMap ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (Pretty k, Storable e, ToField k, ToField e) => EncodeOptions -> MassFractionVectorRecords k e -> IntMap ByteString
encodeWith opts (MassFractionVectorRecords base m) = Data.IntMap.Strict.mapWithKey (\n -> let massFractions = Science.Chemistry.IsotopicLabeling.MassFraction.massFractions base n in Data.Csv.encodeByNameWith opts (toHeader massFractions) . map (uncurry (MassFractionVectorRecord massFractions))) (Science.Chemistry.EMU.HasSize.Class.partitionBySizeWith fst (Data.Map.Strict.toAscList m))

toHeader :: [MassFraction] -> Header
toHeader massFractions = Data.Csv.header $
    cEMUFieldName
  : map Data.Csv.toField massFractions

data IndexedMassFractionVectorRecords i k e = IndexedMassFractionVectorRecords {-# UNPACK #-} !Int (Map i (Map (EMU k) (MassFractionVector e)))
  deriving (Eq, Ord, Read, Show)

data IndexedMassFractionVectorRecord i k e = IndexedMassFractionVectorRecord [MassFraction] i (EMU k) (MassFractionVector e)
  deriving (Eq, Ord, Read, Show)

instance (Pretty k, Storable e, ToField i, ToField k, ToField e) => ToNamedRecord (IndexedMassFractionVectorRecord i k e) where
  toNamedRecord (IndexedMassFractionVectorRecord massFractions ix emu xs) = Data.Csv.namedRecord $
      Data.Csv.namedField cFluxVarFieldName ix
    : Data.Csv.namedField cEMUFieldName emu
    : zipWith Data.Csv.namedField (map Data.Csv.toField massFractions) (Numeric.LinearAlgebra.HMatrix.toList (getConv (getMassFractionVector xs)))

encodeIndexed :: (Pretty k, Storable e, ToField i, ToField k, ToField e) => IndexedMassFractionVectorRecords i k e -> IntMap ByteString
encodeIndexed = encodeIndexedWith Data.Csv.defaultEncodeOptions

encodeIndexedWith :: (Pretty k, Storable e, ToField i, ToField k, ToField e) => EncodeOptions -> IndexedMassFractionVectorRecords i k e -> IntMap ByteString
encodeIndexedWith opts (IndexedMassFractionVectorRecords base m) = Data.IntMap.Strict.mapWithKey (\n -> let massFractions = Science.Chemistry.IsotopicLabeling.MassFraction.massFractions base n in Data.Csv.encodeByNameWith opts (toHeaderIndexed massFractions) . map (uncurry (uncurry . IndexedMassFractionVectorRecord massFractions))) (Science.Chemistry.EMU.HasSize.Class.partitionBySizeWith (fst . snd) (concatMap (uncurry (map . (,))) (Data.Map.Strict.toAscList (Data.Map.Strict.map Data.Map.Strict.toAscList m))))

toHeaderIndexed :: [MassFraction] -> Header
toHeaderIndexed massFractions = Data.Csv.header $
    cFluxVarFieldName
  : cEMUFieldName
  : map Data.Csv.toField massFractions
