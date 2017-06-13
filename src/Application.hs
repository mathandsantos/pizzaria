{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where
import Foundation
import Yesod
import Front
import Pedido
import Usuario

-- Application
mkYesodDispatch "Sitio" resourcesSitio
