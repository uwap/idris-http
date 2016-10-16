module Http

import Http.Uri
import Http.Error
import Http.RawResponse
import Http.Request
import Http.Response
import Data.SortedMap
import Data.Vect
import Network.Socket

%access export

private
sendRequest : Request a -> IO (Either HttpError (RawResponse String))
sendRequest req = do
    --print (resolveRequest req)
    case !(socket AF_INET Stream 0) of
      Left err   => pure (Left $ HttpSocketError err)
      Right sock =>
        case !(connect sock (Hostname host) port) of
          0 =>
            case !(send sock (resolveRequest req)) of
              Left err => pure (Left $ HttpSocketError err)
              Right _  =>
                case !(recv sock 65536) of
                  Left err       => pure (Left $ HttpSocketError err)
                  Right (str, _) => pure (Right (MkRawResponse str))
          err => pure (Left $ HttpSocketError err)
  where
    host : String
    host = uriHost . uriAuth . uri $ req

    port : Int
    port = uriPort . uriAuth . uri $ req

httpRequest : Request a -> IO (Either HttpError (Response String))
httpRequest req = pure $ !(sendRequest req) >>= parseResponse

simpleHttp : Host -> Port -> (path : String) -> IO (Either HttpError (Response String))
simpleHttp host port path = do
  let headers = Data.SortedMap.fromList [("Host", host ++ ":" ++ show port)]
  repl <- sendRequest (MkRequest GET (MkURI "http" (MkURIAuth Nothing Nothing host port) path [] "") "" headers)
  pure (repl >>= parseResponse)
