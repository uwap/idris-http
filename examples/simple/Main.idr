import Http
import Http.Uri
import Http.RawResponse
import Http.Request
import Http.Response
import Network.Socket
import System

uri : String -> Int -> String -> URI
uri host port path = MkURI "http" (MkURIAuth Nothing Nothing host port) path [] ""

toRequest : Method -> String -> Int -> String -> Request
toRequest method host port path = MkRequest method (uri host port path) [] [("Host", host)]

processArgs : List String -> Maybe Request
processArgs (_::method::host::port::path::_) =
  Just (toRequest (cast method) host (cast port) path)
processArgs (_::method::host::path::_) = Just (toRequest (cast method) host 80 path)
processArgs _ = Nothing

partial
main : IO ()
main = do
  args <- getArgs
  case processArgs args of
    Just req => do
      res <- sendRequest req
      case res of
        Left err => print err >>= \_ => putStr "\n"
        Right s => print (responseStatus s) >>= \_ => putStr "\n"
    Nothing => putStrLn "Usage: ./simple method host [port] path"
