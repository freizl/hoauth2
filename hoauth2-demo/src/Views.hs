module Views where

import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Text.Lazy qualified as TL
import Paths_hoauth2_demo
import Text.Mustache
import Text.Parsec.Error
import Types
import Web.Scotty

type CookieUser = String

tpl :: FilePath -> IO (Either ParseError Template)
tpl f =
  -- TODO: can work with cabal v2-run demo-server but not v2-exec
  getDataFileName ("public/templates/" ++ f ++ ".mustache")
    >>= localAutomaticCompile

tplS ::
  FilePath ->
  [DemoAppPerAppSessionData] ->
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
  [DemoAppPerAppSessionData] ->
  -- | List of Idps
  ActionM ()
tplH path xs = do
  s <- liftIO (tplS path xs)
  html s

overviewTpl :: [DemoAppPerAppSessionData] -> ActionM ()
overviewTpl = tplH "index"
