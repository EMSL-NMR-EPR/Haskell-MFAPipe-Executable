module MFAPipe.Csv.Types.Statistics
( StatisticsRecords(..)
, StatisticsRecord(..)
, encode
, encodeWith
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (DefaultOrdered(headerOrder), ToField(), ToNamedRecord(toNamedRecord), EncodeOptions)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Text (Text)
import qualified Data.Text
import           MFAPipe.Csv.Constants

data StatisticsRecords e = StatisticsRecords e ((e, e), Map Text (e, e)) ((e, e), Map Text (e, e)) (e, Map Text e) (e, Map Text e)
  deriving (Eq, Ord, Read, Show)

data StatisticsRecord e = StatisticsRecord e Text (e, e) (e, e) e e
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (StatisticsRecord e) where
  headerOrder _ = Data.Csv.header
    [ cExperimentFieldName
    , cExperimentMeanFieldName
    , cExperimentVarianceFieldName
    , cTolerance
    , cKolmogorovSmirnovDFieldName
    , cKolmogorovSmirnovProbabilityFieldName
    , cWeightedSSEFieldName
    , cWeightedSSTFieldName
    , cWeightedRSquareFieldName
    ]

instance (ToField e, Fractional e) => ToNamedRecord (StatisticsRecord e) where
  toNamedRecord (StatisticsRecord tol experiment (mean, var) (d, p) sse sst) = Data.Csv.namedRecord
    [ Data.Csv.namedField cExperimentFieldName experiment
    , Data.Csv.namedField cExperimentMeanFieldName mean
    , Data.Csv.namedField cExperimentVarianceFieldName var
    , Data.Csv.namedField cTolerance tol
    , Data.Csv.namedField cKolmogorovSmirnovDFieldName d
    , Data.Csv.namedField cKolmogorovSmirnovProbabilityFieldName p
    , Data.Csv.namedField cWeightedSSEFieldName sse
    , Data.Csv.namedField cWeightedSSTFieldName sst
    , Data.Csv.namedField cWeightedRSquareFieldName r2
    ]
    where
      r2 = 1 - (sse / sst)

encode :: (ToField e, Fractional e) => StatisticsRecords e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField e, Fractional e) => EncodeOptions -> StatisticsRecords e -> ByteString
encodeWith opts (StatisticsRecords tol0 (meanVar0, meanVarMap) (p0, pMap) (sse0, sseMap) (sst0, sstMap)) = Data.Csv.encodeDefaultOrderedByNameWith opts (x : xs)
  where
    x = StatisticsRecord tol0 (Data.Text.singleton '*') meanVar0 p0 sse0 sst0
    xs = map (uncurry (\experiment (meanVar, (p, (sse, sst))) -> StatisticsRecord tol0 experiment meanVar p sse sst)) (Data.Map.Strict.toAscList (Data.Map.Strict.intersectionWith (,) meanVarMap (Data.Map.Strict.intersectionWith (,) pMap (Data.Map.Strict.intersectionWith (,) sseMap sstMap))))
