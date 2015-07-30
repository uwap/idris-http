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
||| HttpVersion shall have the value "HTTP\1.0" or "HTTP\1.1"
HttpVersion : Type
HttpVersion = String

||| A data type for requests.
||| A request consists out of a method,
||| a host, a port and a http version.
record Request where
  constructor MkRequest
  method  : Method
  host    : Host
  port    : Port
  version : HttpVersion
