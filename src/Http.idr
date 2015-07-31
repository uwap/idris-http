module Http

import Data.Vect

import Network.Socket

%default total

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

||| A data type for requests.
||| A request consists out of a method,
||| a host, a port, a path, a query and a http version.
record Request where
  constructor MkRequest
  ||| The requests method. Either POST or GET
  method   : Method
  ||| The Host to send the request to
  host     : Host
  port     : Port
  ||| The path that is requested. "/index.html" for example.
  path     : String
  ||| A list of query tuples.
  ||| Setting query to [("v", "1.0")] will append "?v=1.0" to the path
  query    : Vect q (String, String)
  ||| The post data which gets send when method = POST.
  postData : Vect p (String, String)
  ||| The version of the HTTP Request.
  version  : HttpVersion

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
encodeQuery ((k,v) :: xs) = urlEncode k ++ "=" ++ urlEncode v ++ "&" ++ encodeQuery xs

||| The URI of the request.
||| It is the request path concatinated with the query string
||| delivered by encodeQuery.
|||
||| @ req The request to get the URI from
requestUri : (req : Request) -> String
requestUri req = path req ++ "?" ++ encodeQuery (query req)
                -- TODO: Send no question mark when query is empty

||| This is the first line of a Full-Request defined in RFC1945 Section 5.1.
|||
||| @ req The request to get the request line from
requestLine : (req : Request) -> String
requestLine req = show (method req) ++ " " ++ requestUri req ++ " " ++ version req ++ "\r\n"

resolveRequest : Request -> String
resolveRequest req = requestLine req
