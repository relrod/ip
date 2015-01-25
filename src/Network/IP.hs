{-# LANGUAGE ConstraintKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module : Network.IP
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : portable
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

module Network.IP (
  -- Have to list things explicitly until
  -- https://github.com/haskell/haddock/issues/225 is fixed.
  module Network.IP.IPv4
) where

import Network.IP.IPv4
