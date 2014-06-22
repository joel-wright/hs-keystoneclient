{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.Keystone.Client
-- Copyright:   (c) 2014 Joel Wright
-- License:     Apache
-- Maintainer:  Joel Wright <joel.wright@gmail.com>
-- Stability:   Experimental
-- Portability: Portable
--
-- A library for authentication with OpenStack Keystone
--
-- These are the types and operations required for the
-- JSON auth requests.

module Network.Keystone.Client (
    getAuthToken
) where

import Control.Lens hiding ((.=))
import Data.Aeson 
import Data.Aeson.Lens (key, _String)
import Data.Text
import Network.Wreq
import Network.Keystone.Types

authToJSON :: KeystoneAuth -> Value
authToJSON (Auth V2 u) =
    object [ "auth" .= authObj ]
        where
            authObj = object [ "tenantName" .= (String (tenant u))
                             , "passwordCredentials" .= pwdObj ]
            pwdObj = object [ "username" .= (String (username u))
                            , "password" .= (String (password u)) ]
authToJSON (Auth V3 u) = object [] -- TODO

authToTokenURL :: KeystoneAuth -> String
authToTokenURL (Auth V2 u) = unpack $ append (server u) (pack "tokens")
authToTokenURL (Auth V3 u) = "" -- TODO

getAuthToken :: KeystoneAuth -> IO KeystoneToken
getAuthToken a = do
    r <- post (authToTokenURL a) (authToJSON a)
    let
        id = r ^. responseBody . key "access" . key "token" . key "id" . _String
        in
            return (id)
