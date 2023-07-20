module Views where

import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Text.Lazy qualified as TL
import Paths_hoauth2_demo
import Session
import Text.Mustache
import Text.Mustache qualified as M
import Text.Parsec.Error
import Web.Scotty

newtype TemplateData = TemplateData
  { idpSessionData :: [IdpAuthorizationCodeAppSessionData]
  }

instance ToMustache TemplateData where
  toMustache td' =
    M.object
      [ "idps" ~> idpSessionData td'
      ]

tpl :: FilePath -> IO (Either ParseError Template)
tpl f =
  -- TODO: can work with cabal v2-run demo-server but not v2-exec
  getDataFileName ("public/templates/" ++ f ++ ".mustache")
    >>= localAutomaticCompile

tplS ::
  FilePath ->
  [IdpAuthorizationCodeAppSessionData] ->
  IO TL.Text
tplS path xs = do
  template <- tpl path
  case template of
    Left e ->
      return $
        TL.unlines $
          map TL.pack ["can not parse template " ++ path ++ ".mustache", show e]
    Right t' -> return $ TL.fromStrict $ substitute t' (TemplateData $ sortOn idpName xs)

tplH ::
  FilePath ->
  [IdpAuthorizationCodeAppSessionData] ->
  ActionM ()
tplH path xs = do
  s <- liftIO (tplS path xs)
  html s

overviewTpl :: [IdpAuthorizationCodeAppSessionData] -> ActionM ()
overviewTpl = tplH "index"
