module Http.Error

import public Network.Socket

%access public export

data HttpError : Type where
  HttpSocketError : SocketError -> HttpError
  HttpParseError : String -> HttpError

implementation Show HttpError where
  show (HttpSocketError err) = show err
  show (HttpParseError err) = err
