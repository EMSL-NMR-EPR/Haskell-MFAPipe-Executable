module MFAPipe.Csv.Types.EMUReactionNetwork
( EMUReactionNetworkRecord(..)
, encode
, encodeWith
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (DefaultOrdered(headerOrder), ToField(), ToNamedRecord(toNamedRecord), EncodeOptions)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict
import           Data.Monoid (Product(..))
import           MFAPipe.Csv.Constants
import           Science.Chemistry.EMU.EMUReaction (EMUReaction(..))
import           Science.Chemistry.EMU.EMUReactionNetwork (EMUReactionNetwork(..))
import           Science.Chemistry.Types (MixingProbability(..))
import           Text.PrettyPrint.Leijen.Text (Pretty())

data EMUReactionNetworkRecord i k = EMUReactionNetworkRecord i (EMUReaction k)
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (EMUReactionNetworkRecord i k) where
  headerOrder _ = Data.Csv.header
    [ cFluxVarFieldName
    , cEMUReactionSideLeftFieldName
    , cEMUReactionSideRightFieldName
    , cEMUReactionMixingProbabilityFieldName
    ]

instance (ToField i, Pretty k) => ToNamedRecord (EMUReactionNetworkRecord i k) where
  toNamedRecord (EMUReactionNetworkRecord ix (EMUReaction (MixingProbability (Product p)) lhs rhs)) = Data.Csv.namedRecord
    [ Data.Csv.namedField cFluxVarFieldName ix
    , Data.Csv.namedField cEMUReactionSideLeftFieldName lhs
    , Data.Csv.namedField cEMUReactionSideRightFieldName rhs
    , Data.Csv.namedField cEMUReactionMixingProbabilityFieldName (fromRational p :: Double)
    ]

encode :: (ToField i, Pretty k) => EMUReactionNetwork i k -> IntMap ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField i, Pretty k) => EncodeOptions -> EMUReactionNetwork i k -> IntMap ByteString
encodeWith opts = Data.IntMap.Strict.map (Data.Csv.encodeDefaultOrderedByNameWith opts . map (uncurry EMUReactionNetworkRecord)) . getEMUReactionNetwork
