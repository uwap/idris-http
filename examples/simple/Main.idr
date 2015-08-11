import Http
import Http.Uri
import Http.Error
import Http.RawResponse
import Http.Request
import Http.Response
import Data.SortedMap -- Remove after upgrading to idris 0.9.19
import Network.Socket
import System

processArgs : List String -> Maybe (Host, Port, String)
processArgs (_::host::port::path::_) =
  Just (host, (cast port), path)
processArgs (_::host::path::_) = Just (host, 80, path)
processArgs _ = Nothing

instance Cast String String where
  cast = id

printResponse : Response String -> IO ()
printResponse res = do
  putStrLn "Response status:"
  print (responseStatus res)
  putStr "\n\n"

  putStrLn "Headers Received:"
  traverse (\x => do print x; putStr "\n") (Data.SortedMap.toList $ responseHeaders res)
  putStr "\n\n"

  putStrLn "Body:"
  putStr (responseBody res)
  putStr "\n\n"
  
partial
main : IO ()
main =
  case processArgs !getArgs of
    Just (host, port, path) =>
      case !(simpleHttp host port path {a = String}) of
        Left err => print err >>= \_ => putStr "\n"
        Right res => printResponse res
    Nothing => putStrLn "Usage: ./simple host [port] path"
