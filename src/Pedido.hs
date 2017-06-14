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
       

bebz = do
       entidades <- runDB $ selectList [] [Asc BebidaNmBebida] 
       optionsPairs $ fmap (\ent -> (bebidaNmBebida $ entityVal ent, entityKey ent)) entidades
 
widgetBebForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetBebForm x enctype widget y = $(whamletFile "templates/bebida.hamlet")       
       
       
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
                        
getExcluirPizzaR :: PedidoPizzaId -> Handler Html
getExcluirPizzaR ppId = do
                       runDB $ delete ppId
                       redirect HomeR

getExcluirBebidaR :: PedidoBebidaId -> Handler Html
getExcluirBebidaR pbId = do
                     runDB $ delete pbId
                     redirect HomeR            
                     
somarTotal :: [Double] -> Double
somarTotal a = foldl (+) 0 a                     


getListarPedidoR :: PedidoId -> Handler Html
getListarPedidoR ppId = do
                     pedidoP <- runDB $ selectList [PedidoPizzaPedidoId ==. ppId] [] -- Entidade PedidoPizza
                     userPedP <- return $ fmap (pedidoPizzaPizzaId . entityVal) $ pedidoP -- Lista PizzaID
                     
                     userPedBo <- return $ fmap (pedidoPizzaBordaId . entityVal) $ pedidoP -- Lista BordaID
                     listaBordas <- sequence $ map (\borda -> runDB $ get404 borda) userPedBo --LIST ENTIDADE Borda
                     listaVlBorda <- return $ map(\borda -> bordaVlBorda borda) listaBordas
                    
                     listaPizzas <- sequence $ map (\pizza -> runDB $ get404 pizza) userPedP --LIST ENTIDADE PIZZA
                     listaNmPizza <- return $ map(\pizza -> pizzaNmPizza pizza) listaPizzas --Lista com Nomes de Pizzas
                     listaVlPizza <- return $ map(\pizza -> pizzaVlPizza pizza) listaPizzas --Lista com Nomes de Pizzas
               
                     pedidoB <- runDB $ selectList [PedidoBebidaPedidoId ==. ppId] [] -- Entidade PedidoBebida
                     userPedB <- return $ fmap (pedidoBebidaBebidaId . entityVal) $ pedidoB -- Lista BebidaID
                     
                     listaBebidas <- sequence $ map (\bebida -> runDB $ get404 bebida) userPedB --LIST ENTIDADE BEBIDAS
                     listaNmBebida <- return $ map(\bebida -> bebidaNmBebida bebida) listaBebidas --Lista com Nomes de Bebidas
                     listaVlBebida <- return $ map(\bebida -> bebidaVlBebida bebida) listaBebidas --Lista com Nomes de Bebidas
                     
                     let soma = somarTotal(listaVlBebida) + somarTotal(listaVlBorda) + somarTotal(listaVlPizza)
                     
                     
                     listaPiz <- runDB $ selectList [] [Asc PizzaNmPizza]
                     listaBeb <- runDB $ selectList [] [Asc BebidaNmBebida]
                     emailS <- lookupSession "_USER"
                     let certo = do fromJust(emailS)
                     defaultLayout $ do
                         addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                         addStylesheet (StaticR style_css)
                         addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                         addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                         toWidgetHead [hamlet|
                           <meta charset="UTF-8">
                           <meta name="viewport" content="width=device-width, initial-scale=1.0">
                           <meta name="keywords" content="Teste, Haskell">
                         |]                     
                         [whamlet|
                            <header>
                              <nav .navbar .navbar-default>
                                 <div .container-fluid>
                                    <div .navbar-header>
                                       <a href=@{HomeR} .navbar-brand .navbar-left>
                                          Pizzaria
                                       <button type="button" .navbar-toggle .collapsed 
                                       data-toggle="collapse" data-target="#bs-example-navbar-collapsed-1"
                                       aria-expanded="false">
                                       <span .sr-only>
                                          Toggle Navigation
                                       <span .icon-bar>
                                       <span .icon-bar>
                                       <span .icon-bar>
                                    <div .collapse .navbar-collapse #bs-example-navbar-collapsed-1>
                                       <ul .nav .navbar-nav .navbar-right>
                                          <li>
                                             <a href=@{PedidoR} .text-center .nav-custom .ye>
                                                FAZER PEDIDO
                                          <li>
                                             <a href=@{UsAltR} .text-center .nav-custom .ye>
                                                ALTERAR
                                          <li>
                                             <a href=@{LogoutR} .text-center .nav-custom .ye>
                                                (#{certo}) DESLOGAR
                            <main>
                               <div .container .mar-top>
                                 <div .col-xs-6 col-xs-offset-4 .col-sm-6 .col-sm-offset-4 .col-md-6 .col-md-offset-4>
                                    <a .btn .btn-default href=@{PizzaR ppId} role="button">
                                       Adicionar Pizza
                                    <a .btn .btn-default href=@{BebidaR ppId} role="button">
                                       Adicionar Bebida
                                 <table .table .table-bordered .table-striped .text-center .mar-top>
                                    <th .text-center>
                                       Pizza
                                    <th .text-center>
                                       Preço
                                    $forall pizza <- listaPizzas  
                                       <tr>
                                          <td>
                                              #{pizzaNmPizza pizza}
                                          <td>
                                              R$ #{pizzaVlPizza pizza}
                       
                                 <table .table .table-bordered .table-striped .text-center .mar-top>
                                    <th .text-center>
                                       Borda
                                    <th .text-center>
                                       Preço
                                    $forall bor <- listaBordas
                                      <tr> 
                                          <td>
                                              #{bordaNmBorda bor} 
                                          <td>
                                              R$ #{bordaVlBorda bor}
                                                   
                                 <table .table .table-bordered .table-striped .text-center .mar-top>
                                    <th .text-center>
                                       Bebida
                                    <th .text-center>
                                       Preço
                                    $forall beb <- listaBebidas
                                      <tr> 
                                          <td>
                                              #{bebidaNmBebida beb} 
                                          <td>
                                              R$ #{bebidaVlBebida beb}
                                              
                                 <h3 .text-center .mar-top>
                                    Valor total: R$ #{show soma}
                                 
                                 <div .col-xs-5 col-xs-offset-5 .col-sm-5 .col-sm-offset-5 .col-md-5 .col-md-offset-5>
                                    <a .btn .btn-default href=@{ConfirmarPedR ppId} role="button">
                                       Confirmar Pedido
                                             
                         |]         
                         
getConfirmarPedR :: PedidoId -> Handler Html
getConfirmarPedR pedId = do
                           --runDB $ update pedId []
                          defaultLayout [whamlet|
                              <h1> Pedido enviado com sucesso. 
                                 <br>
                                 <a href=@{HomeR} .text-center .ye> 
                                    Voltar
                          |]  