module Http.Uri

record URIAuth where
  constructor MkURIAuth
  uriUsername : Maybe String
  uriPassword : Maybe String
  uriHost : String
  uriPort : Int

record URI where
  constructor MkURI
  uriScheme : String
  uriAuthority : URIAuth
  uriPath : String
  uriQuery : String
  uriFragment : String

uriToString : URI -> String
uriToString u =
  uriScheme u ++ "://" ++ authStr (uriAuthority u) ++
  (uriHost . uriAuthority $ u) ++ ":" ++ show (uriPort . uriAuthority $ u) ++
  uriPath u ++ uriQuery u ++ uriFragment u
  where
    authPassword : URIAuth -> String
    authPassword u' = fromMaybe "" (uriPassword u' >>= return . (":" ++))

    authStrMaybe : URIAuth -> Maybe String
    authStrMaybe u' = return $
      !(uriUsername u') ++ authPassword u' ++ "@"

    authStr : URIAuth -> String
    authStr u' = fromMaybe "" (authStrMaybe u')
