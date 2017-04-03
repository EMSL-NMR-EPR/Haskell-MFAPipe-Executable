module MFAPipe.Csv.Types.Stoichiometry
( DenseRecord(..)
, DenseEncoding(..)
, encode
, encodeWith
, header
) where

import           Control.Applicative (liftA2)
import           Data.Bifunctor (bimap)
import qualified Data.ByteString
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (ToField(), ToNamedRecord(toNamedRecord), EncodeOptions, Header)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Set (Set)
import qualified Data.Set
import           Numeric.LinearAlgebra.HMatrix (Element(), Matrix)
import qualified Numeric.LinearAlgebra.HMatrix
import           Science.Chemistry.Stoichiometry

data DenseRecord i k e = DenseRecord k (Map i e)
  deriving (Eq, Ord, Read, Show)

instance (ToField i, ToField k, ToField e) => ToNamedRecord (DenseRecord i k e) where
  toNamedRecord (DenseRecord k m) = Data.Csv.namedRecord (Data.Csv.namedField Data.ByteString.empty (Data.Csv.toField k) : map (bimap Data.Csv.toField Data.Csv.toField) (Data.Map.Strict.toAscList m))

-- | Should encoding be applied to all chemical species.
data DenseEncoding
  = DenseEncodeIntermediate
  -- ^ Encode intermediates.
  | DenseEncodeReagentProduct
  -- ^ Encode reagents and products.
  | DenseEncodeAll
  -- ^ Encode all.
  deriving (Eq, Ord, Read, Show)

-- | Efficiently serialize a stoichiometric model as a lazy 'Data.ByteString.Lazy.ByteString'.
encode
  :: (Eq i, Element e, ToField i, ToField k, ToField e)
  => DenseEncoding
  -- ^ encoding options for stoichiometric models
  -> Dense i k e
  -- ^ stoichiometric model
  -> ByteString
  -- ^ result
encode = encodeWith Data.Csv.defaultEncodeOptions

-- | Like 'encodeCsvDense', but lets you customize how the CSV data is encoded.
encodeWith
  :: (Eq i, Element e, ToField i, ToField k, ToField e)
  => EncodeOptions
  -- ^ encoding options for CSV files
  -> DenseEncoding
  -- ^ encoding options for stoichiometric models
  -> Dense i k e
  -- ^ stoichiometric model
  -> ByteString
  -- ^ result
encodeWith opts = liftA2 (Data.Csv.encodeByNameWith opts) header . go
  where
    -- | @go opts dense@ is the list of 'DenseRecord's for @dense@ specified by @opts@.
    go :: (Eq i, Element e) => DenseEncoding -> Dense i k e -> [DenseRecord i k e]
    go DenseEncodeIntermediate dense = uncurry (mkDenseRecordList (_denseReactionIndices dense)) (_denseIntermediate dense)
    go DenseEncodeReagentProduct dense = uncurry (mkDenseRecordList (_denseReactionIndices dense)) (_denseReagentProduct dense)
    go DenseEncodeAll dense = liftA2 (++) (go DenseEncodeIntermediate) (go DenseEncodeReagentProduct) dense
    -- | @mkDenseRecordList ixs ks matrix@ is the list of 'DenseRecord's for the given @matrix@,
    -- with the rows and columns given by, respectively, @ks@ and @ixs@.
    mkDenseRecordList :: (Eq i, Element e) => Set i -> Set k -> Matrix e -> [DenseRecord i k e]
    mkDenseRecordList ixs ks matrix = zipWith (\k vector -> DenseRecord k (Data.Map.Strict.fromAscList (zip (Data.Set.toAscList ixs) (Numeric.LinearAlgebra.HMatrix.toList vector)))) (Data.Set.toAscList ks) (Numeric.LinearAlgebra.HMatrix.toRows matrix)

header :: (ToField i) => Dense i k e -> Header
header dense = Data.Csv.header (Data.ByteString.empty : map Data.Csv.toField (Data.Set.toAscList (_denseReactionIndices dense)))
