module MFAPipe.Csv.Types.Flux
( FluxRecords(..)
, FluxRecord(..)
, encode
, encodeWith
, WeightedFluxRecords(..)
, WeightedFluxRecord(..)
, encodeWeighted
, encodeWeightedWith
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (DefaultOrdered(headerOrder), ToField(), ToNamedRecord(toNamedRecord), EncodeOptions)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           MFAPipe.Csv.Constants

data FluxRecords i e = FluxRecords (Map i e)
  deriving (Eq, Ord, Read, Show)

data FluxRecord i e = FluxRecord i e
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (FluxRecord i e) where
  headerOrder _ = Data.Csv.header
    [ cFluxVarFieldName
    , cFluxValueFieldName
    ]

instance (ToField i, ToField e) => ToNamedRecord (FluxRecord i e) where
  toNamedRecord (FluxRecord ix x) = Data.Csv.namedRecord
    [ Data.Csv.namedField cFluxVarFieldName ix
    , Data.Csv.namedField cFluxValueFieldName x
    ]

encode :: (ToField i, ToField e) => FluxRecords i e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField i, ToField e) => EncodeOptions -> FluxRecords i e -> ByteString
encodeWith opts (FluxRecords m) = Data.Csv.encodeDefaultOrderedByNameWith opts (map (uncurry FluxRecord) (Data.Map.Strict.toAscList m))

data WeightedFluxRecords i e = WeightedFluxRecords (Map i (e, e))
  deriving (Eq, Ord, Read, Show)

data WeightedFluxRecord i e = WeightedFluxRecord i e e
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (WeightedFluxRecord i e) where
  headerOrder _ = Data.Csv.header
    [ cFluxVarFieldName
    , cFluxValueFieldName
    , cFluxVarianceFieldName
    ]

instance (ToField i, ToField e, Num e) => ToNamedRecord (WeightedFluxRecord i e) where
  toNamedRecord (WeightedFluxRecord ix x var) = Data.Csv.namedRecord
    [ Data.Csv.namedField cFluxVarFieldName ix
    , Data.Csv.namedField cFluxValueFieldName x
    , Data.Csv.namedField cFluxVarianceFieldName var
    ]

encodeWeighted :: (Ord i, ToField i, ToField e, Num e) => WeightedFluxRecords i e -> ByteString
encodeWeighted = encodeWeightedWith Data.Csv.defaultEncodeOptions

encodeWeightedWith :: (Ord i, ToField i, ToField e, Num e) => EncodeOptions -> WeightedFluxRecords i e -> ByteString
encodeWeightedWith opts (WeightedFluxRecords m) = Data.Csv.encodeDefaultOrderedByNameWith opts (map (uncurry (\ix (x, var) -> WeightedFluxRecord ix x var)) (Data.Map.Strict.toAscList m))
