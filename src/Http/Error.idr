module Http.Error

import Network.Socket

data HttpError : Type where
  HttpSocketError : SocketError -> HttpError
  HttpParseError : String -> HttpError

instance Show HttpError where
  show (HttpSocketError err) = show err
  show (HttpParseError err) = err
