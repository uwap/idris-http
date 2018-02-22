module Http

import public Http.Uri
import public Http.Error
import public Http.RawResponse
import public Http.Request
import public Http.Response
import public Data.SortedMap
import public Data.Vect
import public Network.Socket

%access export

||| Loop until EOF.
||| TODO: Actually check for EOF instead of "all errors"
private
readResp : Socket -> String -> IO String
readResp sock buf = do
  Right (str, t) <- recv sock 65536 | Left err => pure buf
  readResp sock (buf ++ str)

private
sendRequest : Request a -> IO (Either HttpError (RawResponse String))
sendRequest req = do
    Right sock <- socket AF_INET Stream 0 | Left err => pure (Left $ HttpSocketError err)
    0 <- connect sock (Hostname host) port | err => pure (Left $ HttpSocketError err)
    Right _ <- send sock (resolveRequest req) | Left err => pure (Left $ HttpSocketError err)
    str <- readResp sock ""
    pure (Right (MkRawResponse str))
  where
    host : String
    host = uriHost . uriAuth . uri $ req

    port : Int
    port = uriPort . uriAuth . uri $ req

httpRequest : Request a -> IO (Either HttpError (Response String))
httpRequest req = pure $ !(sendRequest req) >>= parseResponse

simpleHttp : Host -> Port -> (path : String) -> IO (Either HttpError (Response String))
simpleHttp host port path = do
  let headers = Data.SortedMap.fromList [("Host", host), ("Connection", "close")]
  repl <- sendRequest (MkRequest GET (MkURI "http" (MkURIAuth Nothing Nothing host port) path [] "") "" headers)
  pure (repl >>= parseResponse)
