module Http

data Method = POST | GET

Host : Type
Host = String

Port : Type
Port = String

HttpVersion : Type
HttpVersion = String

record Request where
  constructor MkRequest
  method  : Method
  host    : Host
  port    : Port
  version : HttpVersion
