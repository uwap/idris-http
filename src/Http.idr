module Http

import Data.Vect

import Network.Socket

||| The HTTP Method which is either POST or GET
data Method = POST | GET

instance Show Method where
  show POST = "POST"
  show GET  = "GET"

||| A String-alias for Hosts.
Host : Type
Host = String

||| A type alias for HTTP Versions.
||| HttpVersion shall have the value "HTTP/1.0" or "HTTP/1.1"
HttpVersion : Type
HttpVersion = String

httpVersion : HttpVersion
httpVersion = "HTTP/1.1"

record URIAuth where
  constructor MkURIAuth
  uriUsername : Maybe String
  uriPassword : Maybe String
  uriHost : String
  uriPort : Int

record URI where
  constructor MkURI
  uriScheme : String
  uriAuthority : URIAuth
  uriPath : String
  uriQuery : String
  uriFragment : String

uriToString : URI -> String
uriToString u =
  uriScheme u ++ "://" ++ authStr (uriAuthority u) ++
  (uriHost . uriAuthority $ u) ++ ":" ++ show (uriPort . uriAuthority $ u) ++
  uriPath u ++ uriQuery u ++ uriFragment u
  where
    authStrMaybe : URIAuth -> Maybe String
    authStrMaybe u' =
      liftA2 (++) (map (++ ":") (uriUsername u')) (map (++ "@") (uriUsername u'))

    authStr u = fromMaybe "" (authStrMaybe u)

||| A data type for requests.
||| A request consists out of a method,
||| a host, a port, a path, a query and a http version.
record Request where
  constructor MkRequest
  ||| The requests method. Either POST or GET.
  method   : Method

  ||| The URI we're attempting to access.
  uri : URI

  ||| The post data which gets send when method = POST.
  postData : Vect p (String, String)

urlEncode : String -> String
urlEncode = id -- TODO: Implement

||| This fiunctions folds a list of query parameters
||| into a query string without prepending '?'.
||| It will also url encode all strings.
|||
||| For example `encodeQuery [("hello", "world"), ("foo", "bar")]`
||| encodes to "hello=world&foo=bar".
|||
||| @ n The length of the vector => the number of query parameters
||| @ q The vector of query param tuples
encodeQuery : (q : Vect n (String, String)) -> String
encodeQuery [] = ""
encodeQuery ((k,v) :: []) = urlEncode k ++ "=" ++ urlEncode v
encodeQuery ((k,v) :: xs) =
  urlEncode k ++ "=" ++ urlEncode v ++ "&" ++ encodeQuery xs

||| The URI of the request.
||| It is the request path concatinated with the query string
||| delivered by encodeQuery.
|||
||| @ req The request to get the URI from
--requestUri : (req : Request) -> String
--requestUri req = path req ++ "?" ++ encodeQuery (query req)
                -- TODO: Send no question mark when query is empty

||| This is the first line of a Full-Request defined in RFC1945 Section 5.1.
|||
||| @ req The request to get the request line from
requestLine : (req : Request) -> String
requestLine req =
  show (method req) ++ " " ++ uriToString (uri req) ++ " " ++ httpVersion ++ "\r\n"

resolveRequest : Request -> String
resolveRequest req = requestLine req

sendRequest : Request -> IO (Either SocketError String)
sendRequest req =
  case !(socket AF_INET Stream 0) of
    Left err   => return (Left err)
    Right sock =>
      case !(connect sock (Hostname (uriHost . uriAuthority . uri $ req)) (uriPort . uriAuthority . uri $ req)) of
        0 =>
          case !(send sock (resolveRequest req)) of
            Left err => return (Left err)
            Right _  =>
              case !(recv sock 65536) of
                Left err       => return (Left err)
                Right (str, _) => return (Right str)
        err => return (Left err)
