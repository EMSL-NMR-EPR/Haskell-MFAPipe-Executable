module MFAPipe.Csv.Types.Residual
( WeightedResidualRecords(..)
, WeightedResidualRecord(..)
, encodeWeighted
, encodeWeightedWith
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (DefaultOrdered(headerOrder), ToField(), ToNamedRecord(toNamedRecord), EncodeOptions)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Text (Text)
import qualified GHC.Exts
import           MFAPipe.Csv.Constants

data WeightedResidualRecords k e = WeightedResidualRecords (Map Text (Map k [((e, e), (e, e))]))
  deriving (Eq, Ord, Read, Show)

data WeightedResidualRecord k e = WeightedResidualRecord Text k (e, e) (e, e)
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (WeightedResidualRecord k e) where
  headerOrder _ = Data.Csv.header
    [ cExperimentFieldName
    , cMeasurementFieldName
    , cMeasurementValueFieldName
    , cMeasurementWeightFieldName
    , cWeightedResidualValueFieldName
    , cWeightedResidualVarianceFieldName
    ]

instance (ToField k, ToField e) => ToNamedRecord (WeightedResidualRecord k e) where
  toNamedRecord (WeightedResidualRecord experiment k (x0, sigma) (x, var)) = Data.Csv.namedRecord
    [ Data.Csv.namedField cExperimentFieldName experiment
    , Data.Csv.namedField cMeasurementFieldName k
    , Data.Csv.namedField cMeasurementValueFieldName x0
    , Data.Csv.namedField cMeasurementWeightFieldName sigma
    , Data.Csv.namedField cWeightedResidualValueFieldName x
    , Data.Csv.namedField cWeightedResidualVarianceFieldName var
    ]

encodeWeighted :: (Ord k, ToField k, ToField e) => WeightedResidualRecords k e -> ByteString
encodeWeighted = encodeWeightedWith Data.Csv.defaultEncodeOptions

encodeWeightedWith :: (Ord k, ToField k, ToField e) => EncodeOptions -> WeightedResidualRecords k e -> ByteString
encodeWeightedWith opts t = Data.Csv.encodeDefaultOrderedByNameWith opts (toList t)

toList :: WeightedResidualRecords k e -> [WeightedResidualRecord k e]
toList (WeightedResidualRecords m) = GHC.Exts.build (\cons nil -> Data.Map.Strict.foldrWithKey (\experiment -> flip (Data.Map.Strict.foldrWithKey (\k -> flip (foldr (\(x0, x) -> cons (WeightedResidualRecord experiment k x0 x)))))) nil m)
