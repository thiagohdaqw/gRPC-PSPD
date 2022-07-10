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
import System.Random(mkStdGen, randomR, StdGen)

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "127.0.0.1"
                            , clientServerPort = 50051
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            , clientAuthority = Nothing
                            }

maxNumbers :: Float
maxNumbers = 500000

calculate :: ([Float], StdGen) -> Float -> ([Float], StdGen)
calculate (list, seed) i = (x : list, newSeed)
    where
        (random, newSeed) = randomR (0, maxNumbers) seed
        x = sqrt((i - (random/2))**2)

numbers :: [Float]
numbers = list
    where
        (list, _) = foldl (calculate) ([], mkStdGen 7777) ([0..maxNumbers-1]::[Float])

main :: IO ()
main = withGRPCClient clientConfig $ \client -> do
  MinMax{..} <- minMaxClient client

  let req = FindRequest (fromList numbers)
  res <-  minMaxFind (ClientNormalRequest req 60 mempty)
  
  case res of
      ClientErrorResponse err -> putStrLn $ show err
      ClientNormalResponse (FindResponse minNumber maxNumber) _ _ _ _
        -> putStrLn $ "MIN = " ++ show minNumber ++ " MAX = " ++ show maxNumber

  return ()
