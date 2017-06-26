module Cat
       ( getCat
       , getCatLink
       ) where

import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.ByteString.Char8 (unpack)
import Debug.Trace (traceShow)
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Simple (httpLBS, getResponseBody)

api_url :: String
api_url = "http://thecatapi.com/api/images/get?format=src&type=gif"

getCat :: IO LBS.ByteString
getCat = getResponseBody <$> (httpLBS $ parseRequest_ api_url)

getCatLink :: IO String
getCatLink = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest api_url
  withResponseHistory request manager $ \his -> do
  	let req = hrFinalRequest his
  	return $ showProtocol req ++ showPort req ++ "//" ++ showHost req ++ showPath req
  	where
  	  showProtocol req = if secure req then "https:" else "http:"
  	  showPort req = if port req == 80 then "" else show (port req)
  	  showHost = unpack . host
  	  showPath = unpack . path
  --let val = find (\(n, _) -> n == hLocation) (responseHeaders response)
  --return . fmap unpack . fmap snd $ traceShow response val



  -- (_, rsp) <- browse $ do
  -- 	setAllowRedirects true
  -- 	request $ getRequest $ api_url
  -- return (take 100 (rspBody rsp))