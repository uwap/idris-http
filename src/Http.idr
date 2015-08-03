module Http

import Http.Uri
import Http.RawResponse
import Http.Request
import Http.Response
import Network.Socket

%access public

data HttpError : Type where
  HttpSocketError : SocketError -> HttpError
  HttpParseError : String -> HttpError

instance Show HttpError where
  show (HttpSocketError err) = show err
  show (HttpParseError err) = err

private
sendRequest : Request -> IO (Either HttpError (RawResponse String))
sendRequest req = do
    --print (resolveRequest req)
    case !(socket AF_INET Stream 0) of
      Left err   => return (Left $ HttpSocketError err)
      Right sock =>
        case !(connect sock (Hostname host) port) of
          0 =>
            case !(send sock (resolveRequest req)) of
              Left err => return (Left $ HttpSocketError err)
              Right _  =>
                case !(recv sock 65536) of
                  Left err       => return (Left $ HttpSocketError err)
                  Right (str, _) => return (Right (MkRawResponse str))
          err => return (Left $ HttpSocketError err)
  where
    host : String
    host = uriHost . uriAuth . uri $ req

    port : Int
    port = uriPort . uriAuth . uri $ req

httpRequest : Request -> IO (Either HttpError Response)
httpRequest req = return $
  case parseResponse <$> !(sendRequest req) of
       Left err => Left err
       Right mr => maybeToEither (HttpParseError "Wasn't able to parse response") mr

simpleHttp : Host -> Port -> (path : String) -> IO (Maybe Response)
simpleHttp host port path = do
  repl <- sendRequest (MkRequest GET (MkURI "http" (MkURIAuth Nothing Nothing host port) path [] "") [] [])
  return $ case repl of
       Right r => parseResponse r
       Left _ => Nothing
