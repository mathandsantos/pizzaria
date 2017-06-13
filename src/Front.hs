{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Front where
import Yesod
import Foundation
import Usuario
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql
import Data.Maybe

-- Sempre devemos usar o defaultLayout
-- pois hamlets, lucius, cassius e julius sao
-- da Monad Widget. A funcao defaultLayout
-- transforma Widgets em Handlers
-- PARA USAR AS IMAGENS, EH NECESSARIO stack clean
-- E DEPOIS stack build para o Yesod criar as funcoes
-- baseadas no arquivos
-- haskell.jpg -> haskell_jpg
-- O lucius e cassius FICAM NO EXECUTAVEL
-- addStylesheet/addScript NAO deixa css/js no EXECUTAVEL

|]

getHomeR :: Handler Html
getHomeR = do
    listaPiz <- runDB $ selectList [] [Asc PizzaNmPizza]
    listaBeb <- runDB $ selectList [] [Asc BebidaNmBebida]
    emailS <- lookupSession "_USER"
    let certo = do fromJust(emailS)  --VARIAVEL CERTO SENDO USADA ONDE ??
    defaultLayout $ do
    setTitle "Pizzaria Haskeller"
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    addStylesheet (StaticR style_css)
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
    toWidgetHead [hamlet|
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <meta name="keywords" content="Teste, Haskell">
    |]