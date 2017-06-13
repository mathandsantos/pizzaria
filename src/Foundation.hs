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

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "config/routes")

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
       
instance Yesod Sitio where
    authRoute _ = Just LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _= return Authorized
    isAuthorized (StaticR _) _= return Authorized
    isAuthorized _ _ = isUser       
       
isUser = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")       
       