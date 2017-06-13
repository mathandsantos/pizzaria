{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Usuario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

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
     