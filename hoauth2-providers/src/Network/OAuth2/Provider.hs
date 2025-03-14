module Network.OAuth2.Provider where

import GHC.Generics (Generic)

data IdpName
  = Auth0
  | AzureAD
  | DropBox
  | Facebook
  | Fitbit
  | GitHub
  | Google
  | Linear
  | LinkedIn
  | Okta
  | Slack
  | StackExchange
  | Twitter
  | Weibo
  | ZOHO
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
