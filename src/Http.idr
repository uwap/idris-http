module Http

import Data.Vect

%default total

||| The HTTP Method which is either POST or GET
data Method = POST | GET

instance Show Method where
  show POST = "POST"
  show GET  = "GET"

||| A String-alias for Hosts.
Host : Type
Host = String

||| Port is an alias for Int.
-- TODO: Maybe depend on the port range 0-65535
Port : Type
Port = Int

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

||| This functions folds a list of query parameters
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

resolveRequest : Request -> String
resolveRequest req = show (method req) ++ " "
                  ++ path req ++ "?" ++ encodeQuery (query req) ++ " "
                  ++ version req
              -- TODO: Implement it properly
