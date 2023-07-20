module Network.OAuth2.Provider.Core.Types where

import GHC.Generics (Generic)

data IdpName
  = Auth0
  | AzureAD
  | DropBox
  | Facebook
  | Fitbit
  | GitHub
  | Google
  | LinkedIn
  | Okta
  | Slack
  | StackExchange
  | Twitter
  | Weibo
  | ZOHO
  deriving (Eq, Ord, Show, Generic)
