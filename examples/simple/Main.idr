import Http
import Http.Uri
import Http.Error
import Http.RawResponse
import Http.Request
import Http.Response
import Data.SortedMap -- Remove after upgrading to idris 0.9.19
import Network.Socket
import System

uri : String -> Int -> String -> URI
uri host port path = MkURI "http" (MkURIAuth Nothing Nothing host port) path [] ""

toRequest : Method -> String -> Int -> String -> Request String
toRequest method host port path = MkRequest method (uri host port path) "" (fromList [("Host", host)])

processArgs : List String -> Maybe (Request String)
processArgs (_::method::host::port::path::_) =
  Just (toRequest (cast method) host (cast port) path)
processArgs (_::method::host::path::_) = Just (toRequest (cast method) host 80 path)
processArgs _ = Nothing

partial
main : IO ()
main =
  case processArgs !getArgs of
    Just req =>
      case !(httpRequest req) of
        Left err => print err >>= \_ => putStr "\n"
        Right res => do
          putStrLn "Response status:"
          print (responseStatus res)
          putStr "\n\n"

          putStrLn "Headers Received:"
          traverse (\x => do print x; putStr "\n") (Data.SortedMap.toList $ responseHeaders res)
          putStr "\n\n"

          putStrLn "Body:"
          print (responseBody res)
          putStr "\n\n"
    Nothing => putStrLn "Usage: ./simple method host [port] path"
