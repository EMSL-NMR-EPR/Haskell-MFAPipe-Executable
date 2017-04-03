module MFAPipe.Csv.Types.ReactionNetwork
( ReactionNetworkRecord(..)
, encode
, encodeWith
) where

import qualified Data.ByteString
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (DefaultOrdered(headerOrder), ToField(), ToNamedRecord(toNamedRecord), EncodeOptions)
import qualified Data.Map.Strict
import           MFAPipe.Csv.Constants
import           Science.Chemistry.Reaction
import           Science.Chemistry.ReactionNetwork
import           Text.PrettyPrint.Leijen.Text (Pretty())

data ReactionNetworkRecord i k a = ReactionNetworkRecord i (Reaction k a)
  deriving (Eq, Ord, Read, Show)

instance DefaultOrdered (ReactionNetworkRecord i k a) where
  headerOrder _ = Data.Csv.header
    [ cFluxVarFieldName
    , cReactionFieldName
    , cReactionTypeFieldName
    ]

instance (Pretty k, Pretty a, ToField i) => ToNamedRecord (ReactionNetworkRecord i k a) where
  toNamedRecord (ReactionNetworkRecord ix reaction) = Data.Csv.namedRecord
    [ Data.Csv.namedField cFluxVarFieldName ix
    , Data.Csv.namedField cReactionFieldName reaction
    , Data.Csv.namedField cReactionTypeFieldName (getReactionType reaction)
    ]

-- | Efficiently serialize a chemical reaction network as a lazy 'Data.ByteString.Lazy.ByteString'.
encode
  :: (Pretty k, Pretty a, ToField i)
  => ReactionNetwork i k a
  -- ^ chemical reaction network
  -> ByteString
  -- ^ result
encode = encodeWith Data.Csv.defaultEncodeOptions

-- | Like 'encodeCsvReactionNetwork', but lets you customize how the CSV data is encoded.
encodeWith
  :: (Pretty k, Pretty a, ToField i)
  => EncodeOptions
  -- ^ encoding options for CSV files
  -> ReactionNetwork i k a
  -- ^ chemical reaction network
  -> ByteString
  -- ^ result
encodeWith opts = Data.Csv.encodeDefaultOrderedByNameWith opts . map (uncurry ReactionNetworkRecord) . Data.Map.Strict.toAscList . getReactionNetwork

getReactionType :: Reaction k a -> Data.ByteString.ByteString
getReactionType (MFAReaction _ _) = cReactionTypeMFA
getReactionType (FBAReaction _ _) = cReactionTypeFBA
{-# INLINE getReactionType #-}
