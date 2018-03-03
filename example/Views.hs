{-# LANGUAGE OverloadedStrings #-}

module Views where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              (sort)
import qualified Data.Text.Lazy         as TL
import           Text.Mustache
import           Text.Parsec.Error
import           Web.Scotty

import           Types

type CookieUser = String

tpl :: FilePath  -> IO (Either ParseError Template)
tpl f = automaticCompile ["./example/templates", "./templates"] (f ++ ".mustache")

tplS :: FilePath
     -> [IDPData]
     -> IO TL.Text
tplS path xs = do
  template <- tpl path
  case template of
    Left e   -> return
                $ TL.unlines
                $ map TL.pack [ "can not parse template " ++ path ++ ".mustache" , show e ]
    Right t' -> return $ TL.fromStrict $ substitute t' (TemplateData $ sort xs)

tplH :: FilePath
     -> [IDPData]
     -> ActionM ()
tplH path xs = do
  s <- liftIO (tplS path xs)
  html s


overviewTpl :: [IDPData] -> ActionM ()
overviewTpl = tplH "index"
