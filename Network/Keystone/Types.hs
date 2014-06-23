{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Keystone.Types
-- Copyright:   (c) 2014 Joel Wright
-- License:     Apache
-- Maintainer:  Joel Wright <joel.wright@gmail.com>
-- Stability:   Experimental
-- Portability: Portable
--
-- A library for authentication with OpenStack Keystone
--
-- These are the types required for the
-- JSON auth requests.

module Network.Keystone.Types (
    KeystoneAuth(..),
    KeystoneUser(..),
    KeystoneVersion(..),
    KeystoneToken
) where

import Data.Text

data KeystoneAuth = Auth KeystoneVersion KeystoneUser

type KeystoneToken = Text

data KeystoneUser = User {
        username :: Text,
        password :: Text,
        tenant :: Text,
        server :: Text
    }

data KeystoneVersion = V2 | V3

