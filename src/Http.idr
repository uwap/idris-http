module Http

import Http.Uri
import Http.Request
import Network.Socket

%access public

data RawResponse a = MkRawResponse a

instance Show a => Show (RawResponse a) where
  show (MkRawResponse a) = show a

instance Functor RawResponse where
  map f (MkRawResponse a) = MkRawResponse (f a)

instance Applicative RawResponse where
  pure = MkRawResponse
  (MkRawResponse f) <*> (MkRawResponse a) = MkRawResponse (f a)

instance Monad RawResponse where
  (MkRawResponse a) >>= f = f a

sendRequest : Request -> IO (Either SocketError (RawResponse String))
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
