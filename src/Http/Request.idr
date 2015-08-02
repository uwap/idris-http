module Http.Request

import Data.Vect
import Http.Uri

%access public

||| The HTTP Method which is either POST or GET
data Method = POST | GET | HEAD

instance Show Method where
  show POST = "POST"
  show GET  = "GET"
  show HEAD = "HEAD"

instance Cast String Method where
  cast "POST" = POST
  cast "HEAD" = HEAD
  cast _      = GET

||| A String-alias for Hosts.
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
record Request where
  constructor MkRequest
  ||| The requests method. Either POST or GET.
  method   : Method

  ||| The URI we're attempting to access.
  uri : URI

  ||| The post data which gets send when method = POST.
  postData : Vect p (String, String)

  ||| Headers to pass along.
  headers : Vect q (String, String)

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

||| This is the first line of a request line defined in RFC7230 Section 3.1.1.
|||
||| @ req The request to get the request line from.
private
requestLine : (req : Request) -> String
requestLine req =
  show (method req) ++ " " ++ uriToString (uri req) ++ " " ++ httpVersion ++ "\r\n"

||| This is a CRLF seperated list of header-fields as defined in RFC7230 Section 3.2.
|||
||| @ req The request to get the header string from.
private
headerFields : (req : Request) -> String
headerFields req = fields (headers req)
  where
    fields [] = ""
    fields ((k,v) :: xs) = k ++ ": " ++ v ++ "\r\n"

||| Convert a request into a string.
||| This follows RFC7230's definition of an HTTP-Message defined in Section 3.
|||
||| @ req The request to convert.
resolveRequest : (req : Request) -> String
resolveRequest req =
  requestLine req ++ requestHeaders req ++ "\r\n"
