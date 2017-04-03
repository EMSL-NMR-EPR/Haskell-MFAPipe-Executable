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

data StatisticsRecords e = StatisticsRecords e ((e, e), Map Text (e, e)) ((e, e), Map Text (e, e)) (Int, Map Text Int) (Int, Map Text Int) (e, Map Text e) (e, Map Text e) (e, Map Text e)
  deriving (Eq, Ord, Read, Show)

data StatisticsRecord e = StatisticsRecord e Text (e, e) (e, e) {-# UNPACK #-} !Int {-# UNPACK #-} !Int e e e
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (StatisticsRecord e) where
  headerOrder _ = Data.Csv.header
    [ cExperimentFieldName
    , cExperimentMeanFieldName
    , cExperimentVarianceFieldName
    , cTolerance
    , cIndependentMeasurementsCount
    , cIndependentFluxesCount
    , cDegreesOfFreedomFieldName
    , cKolmogorovSmirnovDFieldName
    , cKolmogorovSmirnovProbabilityFieldName
    , cWeightedSSEFieldName
    , cWeightedSSTFieldName
    , cWeightedRSquareFieldName
    , cWeightedRSquareAdjustedFieldName
    , cReducedChiSquare
    ]

instance (ToField e, Fractional e) => ToNamedRecord (StatisticsRecord e) where
  toNamedRecord (StatisticsRecord tol experiment (mean, var) (d, p) degreesOfFreedom measurementsCount sse sst chi2) = Data.Csv.namedRecord
    [ Data.Csv.namedField cExperimentFieldName experiment
    , Data.Csv.namedField cExperimentMeanFieldName mean
    , Data.Csv.namedField cExperimentVarianceFieldName var
    , Data.Csv.namedField cTolerance tol
    , Data.Csv.namedField cIndependentMeasurementsCount measurementsCount
    , Data.Csv.namedField cIndependentFluxesCount fluxVarCount
    , Data.Csv.namedField cDegreesOfFreedomFieldName degreesOfFreedom
    , Data.Csv.namedField cKolmogorovSmirnovDFieldName d
    , Data.Csv.namedField cKolmogorovSmirnovProbabilityFieldName p
    , Data.Csv.namedField cWeightedSSEFieldName sse
    , Data.Csv.namedField cWeightedSSTFieldName sst
    , Data.Csv.namedField cWeightedRSquareFieldName r2
    , Data.Csv.namedField cWeightedRSquareAdjustedFieldName adj_r2
    , Data.Csv.namedField cReducedChiSquare chi2
    ]
    where
      fluxVarCount = measurementsCount - degreesOfFreedom
      r2 = 1 - (sse / sst)
      -- <https://en.wikipedia.org/wiki/Coefficient_of_determination#Adjusted_R2>
      adj_r2 = r2 - ((1 - r2) * (fromIntegral (fluxVarCount - 1) / fromIntegral degreesOfFreedom))

encode :: (ToField e, Fractional e) => StatisticsRecords e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField e, Fractional e) => EncodeOptions -> StatisticsRecords e -> ByteString
encodeWith opts (StatisticsRecords tol0 (meanVar0, meanVarMap) (p0, pMap) (degreesOfFreedom0, degreesOfFreedomMap) (measurementsCount0, measurementsCountMap) (sse0, sseMap) (sst0, sstMap) (chi20, chi2Map)) = Data.Csv.encodeDefaultOrderedByNameWith opts (x : xs)
  where
    x = StatisticsRecord tol0 (Data.Text.singleton '*') meanVar0 p0 degreesOfFreedom0 measurementsCount0 sse0 sst0 chi20
    xs = map (uncurry (\experiment (meanVar, (p, (degreesOfFreedom, (measurementsCount, (sse, (sst, chi2)))))) -> StatisticsRecord tol0 experiment meanVar p degreesOfFreedom measurementsCount sse sst chi2)) (Data.Map.Strict.toAscList (Data.Map.Strict.intersectionWith (,) meanVarMap (Data.Map.Strict.intersectionWith (,) pMap (Data.Map.Strict.intersectionWith (,) degreesOfFreedomMap (Data.Map.Strict.intersectionWith (,) measurementsCountMap (Data.Map.Strict.intersectionWith (,) sseMap (Data.Map.Strict.intersectionWith (,) sstMap chi2Map)))))))
