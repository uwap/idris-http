module Http.Request

import public Data.SortedMap
import public Http.Uri

%access export

||| The HTTP Method
public export
data Method = POST | GET | HEAD | PUT | DELETE | CONNECT | OPTIONS | TRACE | PATCH

implementation Show Method where
  show POST = "POST"
  show GET  = "GET"
  show HEAD = "HEAD"
  show PUT     = "PUT"
  show DELETE  = "DELETE"
  show CONNECT = "CONNECT"
  show OPTIONS = "OPTIONS"
  show TRACE   = "TRACE"
  show PATCH   = "PATCH"

implementation Eq Method where
  x == y = show x == show y

implementation Cast String Method where
  cast "POST" = POST
  cast "HEAD" = HEAD
  cast "PUT"     = PUT
  cast "DELETE"  = DELETE
  cast "CONNECT" = CONNECT
  cast "OPTIONS" = OPTIONS
  cast "TRACE"   = TRACE
  cast "PATCH"   = PATCH
  cast _      = GET

||| A String-alias for Hosts.
public export
Host : Type
Host = String

||| A type alias for HTTP Versions.
||| HttpVersion shall have the value "HTTP/1.0" or "HTTP/1.1"
HttpVersion : Type
HttpVersion = String

httpVersion : HttpVersion
httpVersion = "HTTP/1.1"

||| A data type for requests.
||| A request consists out of a method,
||| a host, a port, a path, a query and a http version.
public export
record Request a where
  constructor MkRequest
  ||| The requests method. Either POST or GET.
  method : Method

  ||| The URI we're attempting to access.
  uri : URI

  requestBody : a

  ||| Headers to pass along.
  headers : SortedMap String String

||| This is the first line of a request line defined in RFC7230 Section 3.1.1.
|||
||| @ req The request to get the request line from.
private
requestLine : (req : Request a) -> String
requestLine req =
  show (method req) ++ " " ++ uriToPathString (uri req) ++ " " ++ httpVersion ++ "\r\n"

||| This is a CRLF seperated list of header-fields as defined in RFC7230 Section 3.2.
|||
||| @ req The request to get the header string from.
private
headerFields : (req : Request a) -> String
headerFields req = fields . toList $ headers req
  where
    fields : List (String, String) -> String
    fields [] = ""
    fields ((k,v) :: xs) = k ++ ": " ++ v ++ "\r\n" ++ fields xs

||| Convert a request into a string.
||| This follows RFC7230's definition of an HTTP-Message defined in Section 3.
|||
||| @ req The request to convert.
resolveRequest : (req : Request a) -> String
resolveRequest req =
  requestLine req ++ headerFields req ++ "\r\n"
