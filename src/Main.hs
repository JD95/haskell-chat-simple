{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Concurrent
import qualified Data.ByteString     as B
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

readClientMessages :: Socket -> Chan ByteString -> IO ()
readClientMessages sock chan = do
    message <- B.concat <$> recvUntilEnd sock 1024
    when (message /= "") $ putStrLn message
    when (message /= "quit\r\n\r\n") $ do
      writeChan chan message
      readClientMessages sock chan
    pure ()

sendClientMessages :: Socket -> Chan ByteString -> IO ()
sendClientMessages sock chan =
  forever $ do
    message <- readChan chan
    send sock message

server :: IO ()
server = do
  print "running as server..."
  messages <- newChan @ ByteString
  serve (Host "127.0.0.1") "8000" $ \(sock, remoteAddr) -> do
    putStrLn @Text $ "Connection established from " <> show remoteAddr
    ms <- dupChan messages
    writer <- forkIO $ sendClientMessages sock ms
    readClientMessages sock ms
    killThread writer
    putStrLn @Text $ "closing connection for " <> show remoteAddr

recvUntilEnd :: Socket -> Int -> IO [ByteString]
recvUntilEnd s i = do
  r <- recv s i
  case r of
    Just r' ->
      if "\r\n\r\n" `B.isSuffixOf` r'
        then pure [r']
        else (r' :) <$> recvUntilEnd s i
    Nothing -> pure []

messageEntry :: IO ()
messageEntry = do
  forever $ print =<< getLine

sendMessage :: Socket -> ByteString -> IO ()
sendMessage sock m = send sock (m <> "\r\n\r\n")

recieveServerMessages :: Socket -> IO ()
recieveServerMessages sock =
  forever $ do
    message <- B.concat <$> recvUntilEnd sock 1024
    print message

client :: IO ()
client = do
  print "running as client..."
  putStr @Text "server ip: "
  ip <- pure "127.0.0.1" -- toS <$> getLine
  putStr @Text "server port: "
  port <- pure "8000" -- toS <$> getLine
  connect ip port $ \(sock, remoteAddr) -> do
    putStrLn @Text $ "Connection established to " <> show remoteAddr
    forkIO $ recieveServerMessages sock
    forever $ do
      putStr @Text "> "
      message <- getLine
      sendMessage sock (toS message)
