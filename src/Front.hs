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

    case emailS of
        Nothing -> do
            [whamlet|
              <header>
                  <nav .navbar .navbar-default>
                      <div .container-fluid>
                          <div .navbar-header>
                              <a href=@{HomeR} .navbar-brand .navbar-left>
                                  Pizzaria Haskeller
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
                                     <a href=@{UsuarioR} .text-center .nav-custom .ye>
                                        CADASTRAR
                                  <li>
                                     <a href=@{LoginR} .text-center .nav-custom .ye>
                                        LOGAR
            |]
        Just _ -> do
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
            |]
    [whamlet|
        <main>
           <div .container>
              <h1 .text-center>
                Cardápio de Pizzas e Bebidas
              <h2 .text-center>
                Pizzas
              <table .table .table-bordered .table-striped .text-center>
                <th .text-center>
                   Pizza
                <th .text-center>
                   Preço
                <th .text-center>
                   Descrição
                $forall Entity pid pizza <- listaPiz
                  <tr> 
                      <td>
                          #{pizzaNmPizza pizza} 
                      <td>
                          R$ #{pizzaVlPizza pizza} 
                      <td>
                          #{pizzaDsPizza pizza}
             <div .container>
                <h2 .text-center>
                   Bebidas
                <div .col-xs-12 .col-sm-6 .col-sm-offset-4 .col-md-4 .col-md-offset-4>
                  <table .table .table-bordered .table-striped .text-center>
                     <th .text-center>
                        Bebida
                     <th .text-center>
                        Preço
                     $forall Entity bid bebida <- listaBeb
                        <tr> 
                            <td>
                                #{bebidaNmBebida bebida} 
                            <td>
                                R$ #{bebidaVlBebida bebida} 
        
    |]
