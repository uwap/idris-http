module Http.Uri

import public Data.Vect

%access public export

record URIAuth where
  constructor MkURIAuth
  uriUsername : Maybe String
  uriPassword : Maybe String
  uriHost : String
  uriPort : Int

record URI where
  constructor MkURI
  uriScheme : String
  uriAuth : URIAuth
  uriPath : String
  uriQuery : Vect q (String, String)
  uriFragment : String

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
encodeQuery ((k,v) :: xs) =
  urlEncode k ++ "=" ++ urlEncode v ++ "&" ++ encodeQuery xs

uriToString' : Bool -> URI -> String
uriToString' full u = let query = encodeQuery (uriQuery u) in
  fullIfNeeded ++ uriPath u ++ querySeperator query ++ query ++ uriFragment u
  where
    authPassword : URIAuth -> String
    authPassword u' = fromMaybe "" (uriPassword u' >>= pure . (":" ++))

    authStrMaybe : URIAuth -> Maybe String
    authStrMaybe u' = pure $
      !(uriUsername u') ++ authPassword u' ++ "@"

    authStr : URIAuth -> String
    authStr u' = fromMaybe "" (authStrMaybe u')

    querySeperator : String -> String
    querySeperator "" = ""
    querySeperator _ = "?"

    fullIfNeeded =
      if full
      then uriScheme u ++ "://" ++ authStr (uriAuth u) ++
           (uriHost . uriAuth $ u) ++ ":" ++ show (uriPort . uriAuth $ u)
      else ""

uriToString : URI -> String
uriToString = uriToString' True

uriToPathString : URI -> String
uriToPathString = uriToString' False
