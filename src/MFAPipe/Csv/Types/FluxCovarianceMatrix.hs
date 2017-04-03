module MFAPipe.Csv.Types.FluxCovarianceMatrix
( FluxCovarianceMatrixRecords(..)
, FluxCovarianceMatrixRecord(..)
, encode
, encodeWith
) where

import           Control.Applicative (liftA2)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (ToField(), ToNamedRecord(toNamedRecord), EncodeOptions, Header)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Set (Set)
import qualified Data.Set
import qualified GHC.Exts
import           MFAPipe.Csv.Constants

data FluxCovarianceMatrixRecords i e = FluxCovarianceMatrixRecords (Set i) (Map i (Map i e))
  deriving (Eq, Ord, Read, Show)

data FluxCovarianceMatrixRecord i e = FluxCovarianceMatrixRecord (Set i) i (Map i e)
  deriving (Eq, Ord, Read, Show)

instance (ToField i, ToField e, Ord i, Num e) => ToNamedRecord (FluxCovarianceMatrixRecord i e) where
  toNamedRecord (FluxCovarianceMatrixRecord ixs ix covarMap) = Data.Csv.namedRecord $
      Data.Csv.namedField cFluxVarFieldName ix
    : GHC.Exts.build (\cons nil -> Data.Set.foldr (\ix' -> let covar = Data.Map.Strict.findWithDefault 0 ix' covarMap in cons (Data.Csv.namedField (Data.Csv.toField ix') covar)) nil ixs)

encode :: (ToField i, ToField e, Ord i, Num e) => FluxCovarianceMatrixRecords i e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField i, ToField e, Ord i, Num e) => EncodeOptions -> FluxCovarianceMatrixRecords i e -> ByteString
encodeWith opts = liftA2 (Data.Csv.encodeByNameWith opts) toHeader toList

toHeader :: (ToField i) => FluxCovarianceMatrixRecords i e -> Header
toHeader (FluxCovarianceMatrixRecords ixs _) = Data.Csv.header $
    cFluxVarFieldName
  : GHC.Exts.build (\cons nil -> Data.Set.foldr (\ix -> cons (Data.Csv.toField ix)) nil ixs)

toList :: FluxCovarianceMatrixRecords i e -> [FluxCovarianceMatrixRecord i e]
toList (FluxCovarianceMatrixRecords ixs m) = GHC.Exts.build (\cons nil -> Data.Map.Strict.foldrWithKey (\ix covarMap -> cons (FluxCovarianceMatrixRecord ixs ix covarMap)) nil m)
