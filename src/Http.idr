module Http

import Http.Uri
import Http.Request
import Network.Socket

%access public

data RawResponse = MkRawResponse String

sendRequest : Request -> IO (Either SocketError RawResponse)
sendRequest req = do
    print (resolveRequest req)
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
    host = uriHost . uriAuthority . uri $ req

    port : Int
    port = uriPort . uriAuthority . uri $ req
