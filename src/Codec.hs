module Codec where

import Data.ByteString (ByteString)

------------------------------------------------------------------------

data Codec i o = Codec
  { cDecode :: ByteString -> Maybe i
  , cEncode :: o -> ByteString
  }
