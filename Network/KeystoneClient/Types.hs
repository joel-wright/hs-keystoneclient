{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Network.KeystoneClient.Auth.Types
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

module Network.KeystoneClient.Auth.Types (
    getAuthFromEnv
)

import Data.Aeson
import Data.Text

newtype Auth = Auth Version User
    
data User {
        username :: String,
        password :: String,
        tenant :: String,
        server :: String
    }

data Version = V2 | V3

authToJSON :: Auth -> Value
authToJSON (Auth V2 u) =
    object [ "auth" .= Object authObj ]
        where
            authObj = object [ "tenantName" .= String tenant(u)
                             , "passwordCredentials" .= Object pwdObj ]
            pwdObj = object [ "username" .= String username(u)
                            , "password" .= String password(u) ]
authToJSON (Auth V3 u) = object [] -- TODO

authToTokenURL :: Auth -> String
authToTokenURL (Auth V2 u) = server(u) ++ "tokens"
authToTokenURL (Auth V3 u) = "" -- TODO

getAuthFromEnv :: IO Auth
-- TODO
