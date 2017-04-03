module MFAPipe.Csv.Types.ContributionMatrix
( ContributionMatrixRecords(..)
, ContributionMatrixRecord(..)
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
import           Data.Text (Text)
import qualified GHC.Exts
import           MFAPipe.Csv.Constants

data ContributionMatrixRecords i k e = ContributionMatrixRecords (Set i) (Map Text (Map k [((e, e), Map i e)]))
  deriving (Eq, Ord, Read, Show)

data ContributionMatrixRecord i k e = ContributionMatrixRecord (Set i) Text k (e, e) (Map i e)
  deriving (Eq, Ord, Read, Show)

instance (ToField i, ToField k, ToField e, Ord i, Num e) => ToNamedRecord (ContributionMatrixRecord i k e) where
  toNamedRecord (ContributionMatrixRecord ixs experiment measurement (measurementValue, measurementWeight) contributionMap) = Data.Csv.namedRecord $
      Data.Csv.namedField cExperimentFieldName experiment
    : Data.Csv.namedField cMeasurementFieldName measurement
    : Data.Csv.namedField cMeasurementValueFieldName measurementValue
    : Data.Csv.namedField cMeasurementWeightFieldName measurementWeight
    : GHC.Exts.build (\cons nil -> Data.Set.foldr (\ix -> let contribution = Data.Map.Strict.findWithDefault 0 ix contributionMap in cons (Data.Csv.namedField (Data.Csv.toField ix) contribution)) nil ixs)

encode :: (ToField i, ToField k, ToField e, Ord i, Num e) => ContributionMatrixRecords i k e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField i, ToField k, ToField e, Ord i, Num e) => EncodeOptions -> ContributionMatrixRecords i k e -> ByteString
encodeWith opts = liftA2 (Data.Csv.encodeByNameWith opts) toHeader toList

toHeader :: (ToField i) => ContributionMatrixRecords i k e -> Header
toHeader (ContributionMatrixRecords ixs _) = Data.Csv.header $
    cExperimentFieldName
  : cMeasurementFieldName
  : cMeasurementValueFieldName
  : cMeasurementWeightFieldName
  : GHC.Exts.build (\cons nil -> Data.Set.foldr (\ix -> cons (Data.Csv.toField ix)) nil ixs)

toList :: ContributionMatrixRecords i k e -> [ContributionMatrixRecord i k e]
toList (ContributionMatrixRecords ixs m) = GHC.Exts.build (\cons nil -> Data.Map.Strict.foldrWithKey (\experiment -> flip (Data.Map.Strict.foldrWithKey (\measurement -> flip (foldr (cons . uncurry (ContributionMatrixRecord ixs experiment measurement)))))) nil m)
