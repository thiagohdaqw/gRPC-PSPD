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
import System.CPUTime(getCPUTime)

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

generateNumbers :: StdGen -> [Float]
generateNumbers seed = list
    where
        (list, _) = foldl (calculate) ([], seed) ([0..maxNumbers-1]::[Float])


run :: [Float] -> IO ()
run numbers = withGRPCClient clientConfig $ \client -> do
  MinMax{..} <- minMaxClient client

  let req = FindRequest (fromList numbers)
  res <-  minMaxFind (ClientNormalRequest req 60 mempty)
  
  case res of
      ClientErrorResponse err -> putStrLn $ show err
      ClientNormalResponse (FindResponse minNumber maxNumber) _ _ _ _
        -> putStrLn $ "MIN = " ++ show minNumber ++ " MAX = " ++ show maxNumber

  return ()


main :: IO ()
main = do
  seedTime <- getCPUTime
  let numbers = generateNumbers (mkStdGen (fromIntegral seedTime))

  startTime <- getCPUTime

  run numbers

  endTime <- getCPUTime
  
  let elapsedSeconds = (fromIntegral (endTime - startTime)) / (10^12)

  putStrLn $ "Tempo = " ++ show elapsedSeconds ++ "s"
