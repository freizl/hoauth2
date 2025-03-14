{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module User where

import Data.Text.Lazy qualified as TL
import Network.OAuth2.Provider
import Network.OAuth2.Provider.Auth0 qualified as IAuth0
import Network.OAuth2.Provider.AzureAD qualified as IAzureAD
import Network.OAuth2.Provider.DropBox qualified as IDropBox
import Network.OAuth2.Provider.Facebook qualified as IFacebook
import Network.OAuth2.Provider.Fitbit qualified as IFitbit
import Network.OAuth2.Provider.GitHub qualified as IGitHub
import Network.OAuth2.Provider.Google qualified as IGoogle
import Network.OAuth2.Provider.Linear qualified as ILinear
import Network.OAuth2.Provider.LinkedIn qualified as ILinkedIn
import Network.OAuth2.Provider.Okta qualified as IOkta
import Network.OAuth2.Provider.Slack qualified as ISlack
import Network.OAuth2.Provider.StackExchange qualified as IStackExchange
import Network.OAuth2.Provider.Twitter qualified as ITwitter
import Network.OAuth2.Provider.Weibo qualified as IWeibo
import Network.OAuth2.Provider.ZOHO qualified as IZOHO
import Text.Mustache ((~>))
import Text.Mustache qualified as M
import Prelude hiding (id)

newtype DemoLoginUser = DemoLoginUser
  { loginUserName :: TL.Text
  }
  deriving (Eq, Show)

instance M.ToMustache DemoLoginUser where
  toMustache t' =
    M.object
      ["name" ~> loginUserName t']

class HasDemoLoginUser a where
  type IdpUser a
  toLoginUser :: IdpUser a -> DemoLoginUser

instance HasDemoLoginUser Auth0 where
  type IdpUser Auth0 = IAuth0.Auth0User
  toLoginUser :: IAuth0.Auth0User -> DemoLoginUser
  toLoginUser IAuth0.Auth0User {..} = DemoLoginUser {loginUserName = name}

instance HasDemoLoginUser Google where
  type IdpUser Google = IGoogle.GoogleUser
  toLoginUser :: IGoogle.GoogleUser -> DemoLoginUser
  toLoginUser IGoogle.GoogleUser {..} = DemoLoginUser {loginUserName = name}

instance HasDemoLoginUser ZOHO where
  type IdpUser ZOHO = IZOHO.ZOHOUserResp
  toLoginUser :: IdpUser ZOHO -> DemoLoginUser
  toLoginUser resp =
    let us = IZOHO.users resp
     in case us of
          [] -> DemoLoginUser {loginUserName = "ZOHO: no user found"}
          (a : _) -> DemoLoginUser {loginUserName = IZOHO.fullName a}

instance HasDemoLoginUser AzureAD where
  type IdpUser AzureAD = IAzureAD.AzureADUser
  toLoginUser :: IAzureAD.AzureADUser -> DemoLoginUser
  toLoginUser ouser =
    DemoLoginUser
      { loginUserName = IAzureAD.email ouser <> " " <> IAzureAD.name ouser
      }

instance HasDemoLoginUser Weibo where
  type IdpUser Weibo = IWeibo.WeiboUID
  toLoginUser :: IWeibo.WeiboUID -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = TL.pack $ show $ IWeibo.uid ouser}

instance HasDemoLoginUser DropBox where
  type IdpUser DropBox = IDropBox.DropBoxUser
  toLoginUser :: IDropBox.DropBoxUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IDropBox.displayName $ IDropBox.name ouser}

instance HasDemoLoginUser Facebook where
  type IdpUser Facebook = IFacebook.FacebookUser
  toLoginUser :: IFacebook.FacebookUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IFacebook.name ouser}

instance HasDemoLoginUser Fitbit where
  type IdpUser Fitbit = IFitbit.FitbitUser
  toLoginUser :: IFitbit.FitbitUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IFitbit.userName ouser}

instance HasDemoLoginUser GitHub where
  type IdpUser GitHub = IGitHub.GitHubUser
  toLoginUser :: IGitHub.GitHubUser -> DemoLoginUser
  toLoginUser guser = DemoLoginUser {loginUserName = IGitHub.name guser}

instance HasDemoLoginUser Linear where
  type IdpUser Linear = ILinear.LinearResponse ILinear.LinearUser
  toLoginUser :: ILinear.LinearResponse ILinear.LinearUser -> DemoLoginUser
  toLoginUser resp = DemoLoginUser {loginUserName = ILinear.name (ILinear.getUser resp)}

instance HasDemoLoginUser LinkedIn where
  type IdpUser LinkedIn = ILinkedIn.LinkedInUser
  toLoginUser :: ILinkedIn.LinkedInUser -> DemoLoginUser
  toLoginUser ILinkedIn.LinkedInUser {..} =
    DemoLoginUser
      { loginUserName = name <> " " <> email
      }

instance HasDemoLoginUser Twitter where
  type IdpUser Twitter = ITwitter.TwitterUserResp
  toLoginUser :: ITwitter.TwitterUserResp -> DemoLoginUser
  toLoginUser ITwitter.TwitterUserResp {..} = DemoLoginUser {loginUserName = ITwitter.name twitterUserRespData}

instance HasDemoLoginUser Okta where
  type IdpUser Okta = IOkta.OktaUser
  toLoginUser :: IOkta.OktaUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IOkta.name ouser}

instance HasDemoLoginUser Slack where
  type IdpUser Slack = ISlack.SlackUser
  toLoginUser :: ISlack.SlackUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = ISlack.name ouser}

instance HasDemoLoginUser StackExchange where
  type IdpUser StackExchange = IStackExchange.StackExchangeResp
  toLoginUser :: IStackExchange.StackExchangeResp -> DemoLoginUser
  toLoginUser IStackExchange.StackExchangeResp {..} =
    case items of
      [] -> DemoLoginUser {loginUserName = TL.pack "Cannot find stackexchange user"}
      (user : _) -> DemoLoginUser {loginUserName = IStackExchange.displayName user}
