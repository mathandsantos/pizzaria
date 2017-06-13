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


formPiz :: PedidoId -> Form PedidoPizza
formPiz pedId= renderDivs $ PedidoPizza <$>
             pure pedId <*>
             areq (selectField pizs) "Pizza" Nothing <*>
             areq (selectField bords) "Borda" Nothing


pizs = do
       entidades <- runDB $ selectList [] [Asc PizzaNmPizza] 
       optionsPairs $ fmap (\ent -> (pizzaNmPizza $ entityVal ent, entityKey ent)) entidades
       
bords = do
       entidades <- runDB $ selectList [] [Asc BordaId] 
       optionsPairs $ fmap (\ent -> (bordaNmBorda $ entityVal ent, entityKey ent)) entidades  
       
       
widgetPizzaForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetPizzaForm x enctype widget y = $(whamletFile "templates/pizza.hamlet")  

getPedidoR :: Handler Html
getPedidoR = do
               emailS <- lookupSession "_USER"
               case emailS of
                  Nothing -> do
                     defaultLayout [whamlet| <h1> Usuario não logado.|]
                  Just x -> do
                      usS <- runDB $ getBy $ UniqueEmail x
                      case usS of
                         Nothing -> do
                           defaultLayout [whamlet| <h1> erro.|]
                         Just (Entity usId _) -> do
                            pedId <-runDB $ insert $ Pedido 0.0 False (toSqlKey(fromSqlKey(usId)))
                            let a = do toSqlKey(fromSqlKey(pedId))
                            defaultLayout [whamlet|
                              <h1> #{show a}  -  Inserido com sucesso. 
                            |]
                            redirect $ ListarPedidoR a
                            
getPizzaR :: PedidoId -> Handler Html
getPizzaR pedId = do
                  (widget, enctype) <- generateFormPost $ formPiz pedId
                  defaultLayout $ widgetPizzaForm (PizzaR pedId) enctype widget "Pedidos"                     

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
                    
                    
getBebidaR :: PedidoId -> Handler Html
getBebidaR pedId = do
                  (widget, enctype) <- generateFormPost $ formBeb pedId
                  defaultLayout $ widgetBebForm (BebidaR pedId) enctype widget "Bebidas"
                  
postBebidaR :: PedidoId -> Handler Html
postBebidaR pedId = do
            ((result, _), _) <- runFormPost $ formBeb pedId
            case result of
               FormSuccess (formBeb) -> do
                  a<-runDB $ insert formBeb
                  defaultLayout $ do
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    toWidgetHead [hamlet|
                        <meta charset="UTF-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1.0">
                        <meta name="keywords" content="Teste, Haskell">
                    |]
                    redirect $ ListarPedidoR pedId                  
                    
                    
getExcluirPedidoR :: PedidoId -> Handler Html
getExcluirPedidoR pid = do
                        runDB $ delete pid
                        redirect HomeR                    