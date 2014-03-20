I love the MongoDB raw that Greg Weber put in here earlier. 

I re-worked it for MySQL

``` haskell 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Persist.MySQL.Template where

import Language.Haskell.TH.Syntax hiding (location)


import Database.Persist.MySQL

import Database.Persist.TH
import Control.Applicative

import Database.Persist.Quasi (lowerCaseSettings)

import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import Data.Aeson (Value,ToJSON (..) ,object ,FromJSON (..), (.=),(.:), Value (..) )
import Data.Text
import Data.Typeable
import Data.Word
import Data.Time

{-| YAML FILE LOOKS LIKE ...
     database: <yourdb>
     poolsize: <number of connections>    
     host: <hostname>
     port: <portNumber>
     user: <username>
     password: <password>

|-}

data MySQLConfig =  MySQLConfig {
     user :: Text 
   ,  database :: Text
   ,  password :: Text
   ,  poolsize :: Int    
   ,  host :: Text
   ,  port :: Word16
   }
   deriving (Read, Show,Eq,Typeable)
instance FromJSON MySQLConfig where
    parseJSON (Object tObj) = MySQLConfig <$>
                               tObj .: "user"    <*> 
                               tObj .: "database"    <*> 
                               tObj .: "password"    <*> 
                               tObj .: "poolsize"    <*> 
                               tObj .: "host"    <*> 
                               tObj .: "port"    
                            
    parseJSON _ = fail "Rule: Expecting MongoDB Config Object Received, Other"



instance ToJSON MySQLConfig where
    toJSON (MySQLConfig {..} ) = object [
                                         "user" .= user 
                                      ,  "database" .= database 
                                      ,  "password" .= password 
                                      ,  "poolsize" .= poolsize 
                                      ,  "host" .= host 
                                      ,  "port" .= port ]



readDBConf :: FilePath -> IO (Either String MySQLConfig)
readDBConf fPath = do
	fCont <- BS.readFile fPath
	return $ Y.decodeEither $ fCont



share [mkPersist (mkPersistSettings (ConT ''SqlBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"]
          $(persistFileWith lowerCaseSettings "modelsMySQL")


runDBConf :: MySQLConfig -> (SqlPersistM  b)  -> IO b
runDBConf (MySQLConfig {..}) a = withMySQLPool connInfo  poolsize (\pool -> do
                                                                     runSqlPersistMPool a pool )
    where
      connInfo = ConnectInfo (up host) port (up user) (up password) (up database) [] "" Nothing
      up = unpack
                                                 


```
