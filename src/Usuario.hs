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
