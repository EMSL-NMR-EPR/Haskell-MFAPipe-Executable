{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module MFAPipe.Csv.Types.Info
( encode
, encodeWith
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Csv
import           Data.Csv (DefaultOrdered(headerOrder), ToField(toField), ToNamedRecord(toNamedRecord), EncodeOptions)
import qualified Data.Text
import qualified Data.Text.Encoding
import           Numeric.LevMar (Info(..), StopReason)

instance DefaultOrdered (Info r) where
  headerOrder _ = Data.Csv.header
    [ Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNorm2initE")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNorm2E")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNormInfJacTe")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNorm2Dp")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infMuDivMax")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumIter")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infStopReason")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumFuncEvals")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumJacobEvals")
    , Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumLinSysSolved")
    ]

instance (ToField r) => ToNamedRecord (Info r) where
  toNamedRecord Info{..} = Data.Csv.namedRecord
    [ Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNorm2initE")) infNorm2initE
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNorm2E")) infNorm2E
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNormInfJacTe")) infNormInfJacTe
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNorm2Dp")) infNorm2Dp
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infMuDivMax")) infMuDivMax
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumIter")) infNumIter
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infStopReason")) infStopReason
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumFuncEvals")) infNumFuncEvals
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumJacobEvals")) infNumJacobEvals
    , Data.Csv.namedField (Data.Text.Encoding.encodeUtf8 (Data.Text.pack "infNumLinSysSolved")) infNumLinSysSolved
    ]

instance ToField StopReason where
  toField = toField . show

-- | Efficiently serialize a list of 'Info's as a lazy 'Data.ByteString.Lazy.ByteString'.
encode
  :: (ToField r)
  => [Info r]
  -- ^ 'Info's
  -> ByteString
  -- ^ result
encode = encodeWith Data.Csv.defaultEncodeOptions

-- | Like 'encodeCsvReactionNetwork', but lets you customize how the CSV data is encoded.
encodeWith
  :: (ToField r)
  => EncodeOptions
  -- ^ encoding options for CSV files
  -> [Info r]
  -- ^ 'Info's
  -> ByteString
  -- ^ result
encodeWith = Data.Csv.encodeDefaultOrderedByNameWith
