-- |
-- Module      : Data.ASN1.Pretty
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ASN1.Pretty
    ( pretty
    , PrettyType(..)
    ) where

import           Data.ASN1.Types
import           Data.ASN1.BitArray
import           Data.ByteArray.Encoding (convertToBase, Base(..))
import           Data.ByteString (ByteString)
import           Numeric (showHex)

data PrettyType = Multiline Int -- Offset where to start
                | SingleLine
    deriving (Show,Eq)

-- | Pretty Print a list of ASN.1 element
pretty :: PrettyType -- ^ indent level in space character
       -> [ASN1]     -- ^ stream of ASN1
       -> String
pretty (Multiline at) = prettyPrint at
  where
    indent n = replicate n ' '

    prettyPrint _ []                 = ""
    prettyPrint n (x@(Start _) : xs) = indent n     ++ p id x ++ prettyPrint (n+1) xs
    prettyPrint n (x@(End _) : xs)   = indent (n-1) ++ p id x ++ prettyPrint (n-1) xs
    prettyPrint n (x : xs)           = indent n     ++ p id x ++ prettyPrint n xs

pretty SingleLine = prettyPrint
  where
    prettyPrint []                 = ""
    prettyPrint (x@(Start _) : xs) = p id x ++ "," ++ prettyPrint xs
    prettyPrint (x@(End _) : xs)   = p id x ++ "," ++ prettyPrint xs
    prettyPrint (x : xs)           = p id x ++ "," ++ prettyPrint xs

p :: ([Char] -> t) -> ASN1 -> t
p put (Boolean b)                        = put ("bool: " ++ show b)
p put (IntVal i)                         = put ("int: " ++ showHex i "")
p put (BitString bits)                   = put ("bitstring: " ++ (hexdump $ bitArrayGetData bits))
p put (OctetString bs)                   = put ("octetstring: " ++ hexdump bs)
p put (Null)                             = put "null"
p put (OID is)                           = put ("OID: " ++ show is)
p put (Real d)                           = put ("real: " ++ show d)
p put (Enumerated _)                     = put "enum"
p put (Start Sequence)                   = put "{"
p put (End Sequence)                     = put "}"
p put (Start Set)                        = put "["
p put (End Set)                          = put "]"
p put (Start (Container x y))            = put ("< " ++ show x ++ " " ++ show y)
p put (End (Container x y))              = put ("> " ++ show x ++ " " ++ show y)
p put (ASN1String cs)                    = putCS put cs
p put (ASN1Time TimeUTC time tz)         = put ("utctime: " ++ show time ++ " " ++ show tz)
p put (ASN1Time TimeGeneralized time tz) = put ("generalizedtime: " ++ show time ++ " " ++ show tz)
p put (Other tc tn x)                    = put ("other(" ++ show tc ++ "," ++ show tn ++ "," ++ show x ++ ")")

putCS :: ([Char] -> t) -> ASN1CharacterString -> t
putCS put (ASN1CharacterString UTF8 t)         = put ("utf8string:" ++ show t)
putCS put (ASN1CharacterString Numeric bs)     = put ("numericstring:" ++ hexdump bs)
putCS put (ASN1CharacterString Printable t)    = put ("printablestring: " ++ show t)
putCS put (ASN1CharacterString T61 bs)         = put ("t61string:" ++ show bs)
putCS put (ASN1CharacterString VideoTex bs)    = put ("videotexstring:" ++ hexdump bs)
putCS put (ASN1CharacterString IA5 bs)         = put ("ia5string:" ++ show bs)
putCS put (ASN1CharacterString Graphic bs)     = put ("graphicstring:" ++ hexdump bs)
putCS put (ASN1CharacterString Visible bs)     = put ("visiblestring:" ++ hexdump bs)
putCS put (ASN1CharacterString General bs)     = put ("generalstring:" ++ hexdump bs)
putCS put (ASN1CharacterString UTF32 t)        = put ("universalstring:" ++ show t)
putCS put (ASN1CharacterString Character bs)   = put ("characterstring:" ++ hexdump bs)
putCS put (ASN1CharacterString BMP t)          = put ("bmpstring: " ++ show t)

hexdump :: ByteString -> String
hexdump bs = show (convertToBase Base16 bs :: ByteString)
