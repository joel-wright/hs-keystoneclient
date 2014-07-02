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
-- These are some simple auth operations. 

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
            authObj = object [ "tenantName" .= (String $ pack $ tenant u)
                             , "passwordCredentials" .= pwdObj ]
            pwdObj = object [ "username" .= (String $ pack $ username u)
                            , "password" .= (String $ pack $ password u) ]
authToJSON (Auth V3 u) = object [] -- TODO if necessary

authToTokenURL :: KeystoneAuth -> String
authToTokenURL (Auth V2 u) = (server u) ++ "tokens"
authToTokenURL (Auth V3 u) = "" -- TODO if necessary

getAuthToken :: KeystoneAuth -> IO KeystoneToken
getAuthToken a = do
    r <- postWith opts (authToTokenURL a) (authToJSON a)
    return $ r ^. responseBody . key "access" . key "token" . key "id" . _String
    where
        opts = defaults & header "Accept" .~ ["application/json"]
