module MFAPipe.Csv.Types.ObjectiveFunction
( ObjectiveFunctionRecord(..)
, encode
, encodeWith
) where

import qualified Data.ByteString
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (DefaultOrdered(headerOrder), ToField(), ToNamedRecord(toNamedRecord), EncodeOptions)
import           Data.LinearProgram.Common (Direction(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           MFAPipe.Csv.Constants
import           MFAPipe.Utils
import           Text.PrettyPrint.Leijen.Text (Pretty())

data ObjectiveFunctionRecord i e = ObjectiveFunctionRecord Direction (Map i e) e
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (ObjectiveFunctionRecord i a) where
  headerOrder _ = Data.Csv.header
    [ cObjectiveFunctionFieldName
    , cObjectiveFunctionDirectionFieldName
    , cObjectiveFunctionValueFieldName
    ]

instance (Pretty i, Pretty e, ToField e) => ToNamedRecord (ObjectiveFunctionRecord i e) where
  toNamedRecord (ObjectiveFunctionRecord dir m x) = Data.Csv.namedRecord
    [ Data.Csv.namedField cObjectiveFunctionFieldName (Data.Csv.toField (LinearFunction (Data.Map.Strict.toAscList m)))
    , Data.Csv.namedField cObjectiveFunctionDirectionFieldName (getObjectiveFunctionDirection dir)
    , Data.Csv.namedField cObjectiveFunctionValueFieldName x
    ]

encode :: (Pretty i, Pretty e, ToField e) => Direction -> Map i e -> e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (Pretty i, Pretty e, ToField e) => EncodeOptions -> Direction -> Map i e -> e -> ByteString
encodeWith opts dir m x = Data.Csv.encodeDefaultOrderedByNameWith opts [ObjectiveFunctionRecord dir m x]

getObjectiveFunctionDirection :: Direction -> Data.ByteString.ByteString
getObjectiveFunctionDirection Min = cObjectiveFunctionDirectionMin
getObjectiveFunctionDirection Max = cObjectiveFunctionDirectionMax
