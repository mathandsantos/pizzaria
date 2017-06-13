{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Usuario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing <*>
             areq textField "Nome" Nothing <*>
             areq textField "Telefone" Nothing <*>
             areq textField "CEP" Nothing <*>
             areq textField "Cidade" Nothing <*>
             areq textField "Bairro" Nothing <*>
             areq textField "Endereco" Nothing <*>
             areq textField "Numero" Nothing <*> -- areq é obrigatório
             aopt textField "Complemento" Nothing -- aopt é opcional

formUsuA :: Form Usuario
formUsuA =  renderTable $ Usuario <$>
                areq loginField "" Nothing <*>
                areq senhaField "" Nothing <*>
                
loginField :: Field Handler Text
loginField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#user">
               Usuário: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }                
   
nomeField :: Field Handler Text
nomeField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#name">
               Nome: 
            <input type="text" .form-control placeholder="" value="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }

telField :: Field Handler Text
telField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#tel">
               Telefone: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }   
   
cepField :: Field Handler Text
cepField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#cep">
               CEP: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }
   
cidadeField :: Field Handler Text
cidadeField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#cidade">
               Cidade: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
      
   }
   

bairroField :: Field Handler Text
bairroField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#bairro">
               Bairro: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }

endField :: Field Handler Text
endField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#end">
               Endereco: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }

noField :: Field Handler Text
noField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#no">
               Número: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }
   
coField :: Field Handler Text
coField = Field
  { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
  , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
      [whamlet|
      <br>
      <div .form-group>
            <label for="#co">
              Complemento: 
            <input type="text" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
      |]
  , fieldEnctype = UrlEncoded
  }
  
  senhaField :: Field Handler Text
senhaField = Field
   { fieldParse = \rawVals _ ->
                case rawVals of
                  [a] -> return $ Right $ Just a
                  [] -> return $ Right Nothing
   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
       [whamlet|
       <div .form-group>
            <label for="#user">
               Senha:
            <input type="password" .form-control placeholder="" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField/>
       |]
   , fieldEnctype = UrlEncoded
   }

getLoginR :: Handler Html
getLoginR = do
            (widget, enctype) <- generateFormPost formLogin
            msgComMaybe <- getMessage
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
                   <main>
                    <div .container mar-top>
                       <div .col-md-6 .col-md-offset-4 .mar-top>
                          <fieldset>
                             <h1 .text-center>
                                Login de usuário
                             <form method="post" action=@{LoginR}>
                                 ^{widget}
                                <div .text-center>
                                   <button type="submit" .btn .btn-default .btn-submit>
                                      ENTRAR
                |]
                
getUsuarioR :: Handler Html
getUsuarioR = do
            (widget, enctype) <- generateFormPost formUsuA
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
                   <main>
                    <div .container mar-top>
                       <div .col-md-6 .col-md-offset-4 .mar-top>
                          <fieldset>
                             <h1 .text-center>
                                Cadastro de usuário:
                             <form method="post" action=@{UsuarioR}>
                                 ^{widget}
                                <div .text-center>
                                   <button type="submit" .btn .btn-default .btn-submit>
                                      CADASTRAR
                   |]  