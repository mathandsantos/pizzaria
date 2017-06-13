{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
             
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Usuario
    email Text
    senha Text
    nmUsuario Text
    telefone Text
    cep Text    
    cidade Text
    bairro Text
    rua Text
    numero Text
    complemento Text Maybe 
    UniqueEmail email
    deriving Show

Pedido
    vlPedido Double
    icPedido Bool 
    usuarioId UsuarioId
    deriving Show
    
Pizza
    nmPizza Text
    vlPizza Double
    dsPizza Text
    deriving Show

Borda
    nmBorda Text
    vlBorda Double
    deriving Show
        
PedidoPizza
    pedidoId PedidoId
    pizzaId PizzaId
    bordaId BordaId
    deriving Show

Bebida
    nmBebida Text
    vlBebida Double
    deriving Show

PedidoBebida
    pedidoId PedidoId
    bebidaId BebidaId
    deriving Show
|]

