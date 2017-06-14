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


formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
               areq loginField "" Nothing <*> 
               areq senhaField "" Nothing
               
               
formUsuA :: Form Usuario
formUsuA =  renderTable $ Usuario <$>
                areq loginField "" Nothing <*>
                areq senhaField "" Nothing <*>
                areq nomeField "" Nothing <*>
                areq telField "" Nothing <*>
                areq cepField "" Nothing <*>
                areq cidadeField "" Nothing <*>
                areq bairroField "" Nothing <*>
                areq endField "" Nothing <*>
                areq noField "" Nothing <*> -- areq é obrigatório
                aopt coField "" Nothing
                
                
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


postLoginR :: Handler Html
postLoginR = do
                ((result, _), _) <- runFormPost formLogin
                case result of
                    FormSuccess (email,senha) -> do
                       temUsu <- runDB $ selectFirst [UsuarioEmail ==. email,UsuarioSenha ==. senha] []
                       case temUsu of
                           Nothing -> do
                               setMessage [shamlet| <p> Usuário ou senha inválido |]
                               redirect LoginR
                           Just _ -> do
                               setSession "_USER" email
                               defaultLayout [whamlet| Usuário autenticado!|]
                               redirect HomeR
                    _ -> redirect LoginR

                
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
                   
postUsuarioR :: Handler Html
postUsuarioR = do
                ((result, _), _) <- runFormPost formUsuA
                case result of
                    FormSuccess usu -> do
                       usuLR <- runDB $ insertBy usu
                       case usuLR of
                           Left _ -> redirect UsuarioR
                           Right _ -> defaultLayout $ do
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
                                              <h1>
                                                #{usuarioNmUsuario usu} Inserido com sucesso. 
                                              <br>
                                                 <a href=@{HomeR} .text-center .ye> 
                                                    Voltar
                                          |] 
                    _ -> redirect UsuarioR       
                    
getUsAltR :: Handler Html
getUsAltR = do    
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
                                                     <a href=@{PedidoR} .text-center .nav-custom .ye>
                                                        FAZER PEDIDO
                                                     <li>
                                                      <a href=@{UsAltR} .text-center .nav-custom .ye>
                                                        ALTERAR
                                                      <li>
                                                         <a href=@{LogoutR} .text-center .nav-custom .ye>
                                                             DESLOGAR
                               <main>
                                <div .container mar-top>
                                   <div .col-md-6 .col-md-offset-4 .mar-top>
                                      <fieldset>
                                         <h1 .text-center>
                                            Alterar dados:
                                         <form method="post" action=@{UsAltR}>
                                             ^{widget}
                                            <div .text-center>
                                               <button type="submit" .btn .btn-default .btn-submit>
                                                  Alterar
                               |]    

postUsAltR :: Handler Html
postUsAltR = do
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
                         --pedId <-runDB $ insert $ Pedido 0.0 False (toSqlKey(fromSqlKey(usId)))
                         ((result, _), _) <- runFormPost formUsuA
                         case result of
                             FormSuccess usu -> do
                                -- listaUsu <- runDB $ selectList [UsuarioId ==. usId] []
                                runDB $ update usId  [UsuarioNmUsuario =. usuarioNmUsuario usu, UsuarioTelefone =. usuarioTelefone usu]
                                defaultLayout [whamlet|
                                    <h1> #{usuarioNmUsuario usu} alterado com sucesso. 
                                       <br>
                                       <a href=@{HomeR} .text-center .ye> 
                                          Voltar
                                |]
                             _ -> redirect UsAltR 

getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_USER"
    redirect HomeR                    