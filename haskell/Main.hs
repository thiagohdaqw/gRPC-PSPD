{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Minmax
import Network.GRPC.HighLevel.Generated
import Data.Vector(fromList)

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "127.0.0.1"
                            , clientServerPort = 50051
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            , clientAuthority = Nothing
                            }


main :: IO ()
main = withGRPCClient clientConfig $ \client -> do
  MinMax{..} <- minMaxClient client

  let req = FindRequest (fromList [1, 2, 3, 4, 5])
  res <-  minMaxFind (ClientNormalRequest req 60 mempty)
  
  case res of
      ClientErrorResponse err -> putStrLn $ show err
      ClientNormalResponse (FindResponse minNumber maxNumber) _ _ _ _
        -> putStrLn $ "MIN = " ++ show minNumber ++ " MAX = " ++ show maxNumber

  return ()
