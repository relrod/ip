{-# LANGUAGE ConstraintKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module : Network.IP.IPv4
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : portable
--
-- This module deals only with the encoding, parsing, and transformation of IPv4
-- addresses.
----------------------------------------------------------------------------

module Network.IP.IPv4 where

import Control.Applicative
import Data.Bits
import Data.Char (digitToInt, intToDigit)
import Data.List (dropWhileEnd, foldl')
import Data.Word
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

-- | A representation (in 'Word32' form) of an IPv4 address.
data IPv4 = IPv4 !Word32 deriving (Eq, Ord)

-- | A representation (in 'Word32' form) of an IPv4 subnet.
data IPv4SubnetMask = IPv4SubnetMask !Word32 deriving (Eq, Ord)

newtype IPv4Subnet = IPv4Subnet (IPv4, IPv4SubnetMask) deriving (Eq, Ord)

instance Show IPv4 where
  show (IPv4 addr) = word32ToIp addr

instance Show IPv4SubnetMask where
  show (IPv4SubnetMask addr) = word32ToIp addr

instance Show IPv4Subnet where
  show (IPv4Subnet (IPv4 addr, IPv4SubnetMask netmask)) =
    word32ToIp addr ++ "/" ++
    (show . length . drop 1 . dropWhileEnd (== '0') . map intToDigit . toBinary $ netmask)

toBinary :: Word32 -> [Int]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [fromIntegral n `rem` 2]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

word32ToIp :: Word32 -> String
word32ToIp addr = show ((addr `shiftR` 24) .&. 0xff) ++ "." ++
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

cidrToSubnetMask :: Integer -> IPv4SubnetMask
cidrToSubnetMask c =
  let ones  = replicate (fromIntegral c) '1'
      zeros = replicate (32 - fromIntegral c) '0'
  in IPv4SubnetMask . fromIntegral . toDec $ ones ++ zeros

-- | Parses an IPv4 in CIDR notation into an 'IPv4Subnet' type.
ipv4CidrParser :: IpParseConstraint m => m IPv4Subnet
ipv4CidrParser = do
  netAddress <- ipv4Parser
  _ <- char '/'
  subnetMask <- decimal
  return $ IPv4Subnet (netAddress, cidrToSubnetMask subnetMask)

ipsInSubnet :: IPv4Subnet -> [IPv4]
ipsInSubnet (IPv4Subnet (IPv4 addr, IPv4SubnetMask netmask)) =
  let numIps = toDec . map (\ '0' -> '1') . dropWhile (== '1') . drop 1 . map intToDigit . toBinary $ netmask
  in map (IPv4 . fromIntegral) . take (fromIntegral numIps) $ iterate (+ (1 :: Integer)) (fromIntegral addr)
