{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Disco
    ( run
    ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text
import Network.Discord
import Pipes

import Cat (getCat, getCatLink)

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} txt = fetch' $ CreateMessage chan txt Nothing

upload :: Message -> LBS.ByteString -> Effect DiscordM ()
upload msg bs = fetch' $ UploadFile (messageChannel msg) "cat.gif" bs


run :: String -> IO ()
run token = runBot (Bot token) $ do
  with ReadyEvent $ \_ ->
    liftIO . putStrLn $ "Connected"

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("!cat" == messageContent && (not . userIsBot $ messageAuthor)) $
     liftIO getCat >>= upload msg 

    when ("!cat link" == messageContent && (not . userIsBot $ messageAuthor)) $
      liftIO getCatLink >>= reply msg . pack