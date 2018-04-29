module MFAPipe.Csv.Types.Parameters
  ( ParametersRecords(..)
  , ParametersRecord(..)
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

data ParametersRecords i e = ParametersRecords (Set i) [Map i e]
  deriving (Eq, Ord, Read, Show)

data ParametersRecord i e = ParametersRecord (Set i) {-# UNPACK #-} !Int (Map i e)
  deriving (Eq, Ord, Read, Show)

instance (ToField i, ToField e, Ord i, Num e) => ToNamedRecord (ParametersRecord i e) where
  toNamedRecord (ParametersRecord ixs ix xs) = Data.Csv.namedRecord $
      Data.Csv.namedField cIterationIndexFieldName ix
    : GHC.Exts.build (\cons nil -> Data.Set.foldr (\ix' -> let x = Data.Map.Strict.findWithDefault 0 ix' xs in cons (Data.Csv.namedField (Data.Csv.toField ix') x)) nil ixs)

encode :: (ToField i, ToField e, Ord i, Num e) => ParametersRecords i e -> ByteString
encode = encodeWith Data.Csv.defaultEncodeOptions

encodeWith :: (ToField i, ToField e, Ord i, Num e) => EncodeOptions -> ParametersRecords i e -> ByteString
encodeWith opts = liftA2 (Data.Csv.encodeByNameWith opts) toHeader toList

toHeader :: (ToField i) => ParametersRecords i e -> Header
toHeader (ParametersRecords ixs _xss) = Data.Csv.header $
    cIterationIndexFieldName
  : GHC.Exts.build (\cons nil -> Data.Set.foldr (\ix -> cons (Data.Csv.toField ix)) nil ixs)

toList :: ParametersRecords i e -> [ParametersRecord i e]
toList (ParametersRecords ixs xss) = zipWith (ParametersRecord ixs) (enumFromThen 0 1) xss
