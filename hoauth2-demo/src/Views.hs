{-# LANGUAGE OverloadedStrings #-}

module Views where

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import qualified Data.Text.Lazy as TL
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
  [DemoAppEnv] ->
  IO TL.Text
tplS path xs = do
  template <- tpl path
  case template of
    Left e ->
      return $
        TL.unlines $
          map TL.pack ["can not parse template " ++ path ++ ".mustache", show e]
    Right t' -> return $ TL.fromStrict $ substitute t' (TemplateData $ sort xs)

tplH ::
  FilePath ->
  [DemoAppEnv] ->
  ActionM ()
tplH path xs = do
  s <- liftIO (tplS path xs)
  html s

overviewTpl :: [DemoAppEnv] -> ActionM ()
overviewTpl = tplH "index"
