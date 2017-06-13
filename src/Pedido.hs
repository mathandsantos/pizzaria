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