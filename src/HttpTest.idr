import Network.Socket
import Http
import Http.Uri

main : IO ()
main = do
  let a = MkURIAuth Nothing Nothing "4.da.gd" 80
  let b = MkURI "http" a "/ip" "" ""
  let c = MkRequest GET b [] [("Host", "4.da.gd")]
  res <- sendRequest c
  case res of
    Left err => print "error"
    Right s => print s
