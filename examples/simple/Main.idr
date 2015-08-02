import Network.Socket
import Http
import Http.Uri
import Http.RawResponse
import Http.Request
import Http.Response

main : IO ()
main = do
  let a = MkURIAuth Nothing Nothing "4.da.gd" 80
  let b = MkURI "http" a "/ip" "" ""
  let c = MkRequest HEAD b [] [("Host", "4.da.gd")]
  res <- sendRequest c
  case res of
    Left err => print "error"
    Right s => print (responseStatus s)
