module Http

||| The HTTP Method which is either POST or GET
data Method = POST | GET

||| A String-alias for Hosts.
Host : Type
Host = String

||| Port is an alias for Int.
-- Todo: Maybe depend on the port range 0-65535
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
  query    : q ** Vect q (String, String)
  ||| The post data which gets send when method = POST.
  postData : q ** Vect q (String, String)
  ||| The version of the HTTP Request.
  version  : HttpVersion
