{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module User where

import Data.Text.Lazy qualified as TL
import Network.OAuth2.Experiment
import Network.OAuth2.Provider.Auth0 qualified as IAuth0
import Network.OAuth2.Provider.AzureAD qualified as IAzureAD
import Network.OAuth2.Provider.Dropbox qualified as IDropbox
import Network.OAuth2.Provider.Facebook qualified as IFacebook
import Network.OAuth2.Provider.Fitbit qualified as IFitbit
import Network.OAuth2.Provider.Github qualified as IGithub
import Network.OAuth2.Provider.Google qualified as IGoogle
import Network.OAuth2.Provider.Linkedin qualified as ILinkedin
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
  toLoginUser :: IdpUserInfo a -> DemoLoginUser

instance HasDemoLoginUser IAuth0.Auth0 where
  toLoginUser :: IAuth0.Auth0User -> DemoLoginUser
  toLoginUser IAuth0.Auth0User {..} = DemoLoginUser {loginUserName = name}

instance HasDemoLoginUser IGoogle.Google where
  toLoginUser :: IGoogle.GoogleUser -> DemoLoginUser
  toLoginUser IGoogle.GoogleUser {..} = DemoLoginUser {loginUserName = name}

instance HasDemoLoginUser IZOHO.ZOHO where
  toLoginUser :: IdpUserInfo IZOHO.ZOHO -> DemoLoginUser
  toLoginUser resp =
    let us = IZOHO.users resp
     in case us of
          [] -> DemoLoginUser {loginUserName = "ZOHO: no user found"}
          (a : _) -> DemoLoginUser {loginUserName = IZOHO.fullName a}

instance HasDemoLoginUser IAzureAD.AzureAD where
  toLoginUser :: IAzureAD.AzureADUser -> DemoLoginUser
  toLoginUser ouser =
    DemoLoginUser
      { loginUserName = IAzureAD.email ouser <> " " <> IAzureAD.name ouser
      }

instance HasDemoLoginUser IWeibo.Weibo where
  toLoginUser :: IWeibo.WeiboUID -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = TL.pack $ show $ IWeibo.uid ouser}

instance HasDemoLoginUser IDropbox.Dropbox where
  toLoginUser :: IDropbox.DropboxUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IDropbox.displayName $ IDropbox.name ouser}

instance HasDemoLoginUser IFacebook.Facebook where
  toLoginUser :: IFacebook.FacebookUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IFacebook.name ouser}

instance HasDemoLoginUser IFitbit.Fitbit where
  toLoginUser :: IFitbit.FitbitUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IFitbit.userName ouser}

instance HasDemoLoginUser IGithub.Github where
  toLoginUser :: IGithub.GithubUser -> DemoLoginUser
  toLoginUser guser = DemoLoginUser {loginUserName = IGithub.name guser}

instance HasDemoLoginUser ILinkedin.Linkedin where
  toLoginUser :: ILinkedin.LinkedinUser -> DemoLoginUser
  toLoginUser ILinkedin.LinkedinUser {..} =
    DemoLoginUser
      { loginUserName = localizedFirstName <> " " <> localizedLastName
      }

instance HasDemoLoginUser ITwitter.Twitter where
  toLoginUser :: ITwitter.TwitterUserResp -> DemoLoginUser
  toLoginUser ITwitter.TwitterUserResp {..} = DemoLoginUser {loginUserName = ITwitter.name twitterUserRespData}

instance HasDemoLoginUser IOkta.Okta where
  toLoginUser :: IOkta.OktaUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IOkta.name ouser}

instance HasDemoLoginUser ISlack.Slack where
  toLoginUser :: ISlack.SlackUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = ISlack.name ouser}

instance HasDemoLoginUser IStackExchange.StackExchange where
  toLoginUser :: IStackExchange.StackExchangeResp -> DemoLoginUser
  toLoginUser IStackExchange.StackExchangeResp {..} =
    case items of
      [] -> DemoLoginUser {loginUserName = TL.pack "Cannot find stackexchange user"}
      (user : _) -> DemoLoginUser {loginUserName = IStackExchange.displayName user}
