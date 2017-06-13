{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Pedido where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text hiding (map,foldl)
import Usuario
import Data.Maybe
import Database.Persist.Postgresql

             
formBeb :: PedidoId -> Form PedidoBebida
formBeb pedId= renderDivs $ PedidoBebida <$>
             pure pedId <*>
             areq (selectField bebz) "Bebida" Nothing 

bebz = do
       entidades <- runDB $ selectList [] [Asc BebidaNmBebida] 
       optionsPairs $ fmap (\ent -> (bebidaNmBebida $ entityVal ent, entityKey ent)) entidades
       
widgetBebForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetBebForm x enctype widget y = $(whamletFile "templates/bebida.hamlet")       



postPizzaR :: PedidoId -> Handler Html
postPizzaR pedId = do
            ((result, _), _) <- runFormPost $ formPiz pedId
            case result of
               FormSuccess (formPiz) -> do
                  a<-runDB $ insert formPiz
                  --let b = do toSqlKey(fromSqlKey(a))
                  defaultLayout $ do
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    toWidgetHead [hamlet|
                        <meta charset="UTF-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1.0">
                        <meta name="keywords" content="Teste, Haskell">
                    |]
                    [whamlet|
                        <h1> #{show a}  -  Inserido com sucesso. 
                    |]
                    redirect $ ListarPedidoR pedId