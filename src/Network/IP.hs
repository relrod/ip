{-# LANGUAGE ConstraintKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module : Network.IP
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : lens
--
-- This library provides functions and types for handling/dealing with IP
-- addressing and subnetting, along with parsing them.
--
-- It is loosely based on Twitter\'s
-- <https://github.com/twitter/util/blob/master/util-core/src/main/scala/com/twitter/util/NetUtil.scala util>
-- and sebnow's
-- <https://github.com/sebnow/haskell-network-address haskell-network-address>
-- but gets away from usage of 'read' and hopefully fixes some other bugs.
----------------------------------------------------------------------------

module Network.IP where

import Control.Applicative
import Data.Bits
import Data.Word
--import qualified Data.Attoparsec.ByteString as A
--import qualified Data.ByteString.Char8 as B
--import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

-- | A representation (in 'Word32' form) of an IPv4 address.
data IPv4 = IPv4 !Word32 deriving (Eq, Ord)

instance Show IPv4 where
  show (IPv4 addr) = show ((addr `shiftR` 24) .&. 0xff) ++ "." ++
                     show ((addr `shiftR` 16) .&. 0xff) ++ "." ++
                     show ((addr `shiftR` 8) .&. 0xff) ++ "." ++
                     show (addr .&. 0xff)

type IpParseConstraint m = (Monad m, TokenParsing m)

-- | Parses an individual octet of an IPv4 IP address.
ipv4Octet :: IpParseConstraint m => m Integer
ipv4Octet = do
  octet <- decimal
  if octet >= 0 && octet <= 255
    then return  octet
    else unexpected "Octet is not in range [0, 255]"

-- | Parses an IPv4 address into an 'IPv4' type.
ipv4Parser :: IpParseConstraint m => m IPv4
ipv4Parser = do
  octet1 <- (\x -> x * (256 ^ (3 :: Integer))) <$> ipv4Octet
  _ <- char '.'
  octet2 <- (\x -> x * (256 ^ (2 :: Integer))) <$> ipv4Octet
  _ <- char '.'
  octet3 <- (* 256) <$> ipv4Octet
  _ <- char '.'
  octet4 <- ipv4Octet
  return $ IPv4 (fromIntegral $ octet1 + octet2 + octet3 + octet4)
