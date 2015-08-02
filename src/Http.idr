module Http

import Http.Uri
import Http.RawResponse
import Http.Request
import Http.Response
import Network.Socket

%access public

sendRequest : Request -> IO (Either SocketError (RawResponse String))
sendRequest req = do
    --print (resolveRequest req)
    case !(socket AF_INET Stream 0) of
      Left err   => return (Left err)
      Right sock =>
        case !(connect sock (Hostname host) port) of
          0 =>
            case !(send sock (resolveRequest req)) of
              Left err => return (Left err)
              Right _  =>
                case !(recv sock 65536) of
                  Left err       => return (Left err)
                  Right (str, _) => return (Right (MkRawResponse str))
          err => return (Left err)
  where
    host : String
    host = uriHost . uriAuth . uri $ req

    port : Int
    port = uriPort . uriAuth . uri $ req

simpleHttp : Host -> Port -> (path : String) -> IO (Maybe Response)
simpleHttp host port path = do
  repl <- sendRequest (MkRequest GET (MkURI "http" (MkURIAuth Nothing Nothing host port) path [] "") [] [])
  return $ case repl of
       Right r => parseResponse r
       Left _ => Nothing
