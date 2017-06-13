{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Yesod
import Yesod.Static
import Foundation
import Application
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d3ks518baa9jb3 host=ec2-107-21-99-176.compute-1.amazonaws.com user=abkvxufbecnexf password=28bc9dbb7cc82caa3bfc86135711d9f8837151f561491c339226580cf4e23dbc port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Sitio t pool)