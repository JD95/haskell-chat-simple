{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Lens.Micro.Platform
import           Network.Simple.TCP
import           Options.Applicative
import           Prelude             ()
import           Protolude           hiding (option)

data Options = Options { asClient :: Bool }

opts :: Parser Options
opts = Options <$> option auto
     ( long "client"
    <> short 'c'
    <> metavar "CLIENT"
    <> help "run as the client"
    <> showDefault
    <> value False
      )

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "simple chat server"
  <> header "comp-51"
  )

main :: IO ()
main = do
  options <- execParser optsInfo
  print (asClient options)

server :: IO ()
server = do
  print "running as server..."
  serve (Host "127.0.0.1") "8000" $ \(sock, remoteAddr) -> do
    putStrLn @Text $ "Connection established from " <> show remoteAddr

client :: IO ()
client = do
  print "running as client..."
  putStr @Text "server ip: "
  ip <- toS <$> getLine
  putStr @Text "server port: "
  port <- toS <$> getLine
  connect ip port $ \(sock, remoteAddr) -> do
    putStrLn @Text $ "Connection established to " <> show remoteAddr
