module Http.Response

import Data.Vect
import Http.RawResponse

%access public

record ResponseStatus where
  constructor MkResponseStatus
  responseStatusVersion : String
  responseStatusCode : String  -- TODO: Int
  responseStatusComment : String

instance Show ResponseStatus where
  show (MkResponseStatus ver code cmt) =
    "MkRawResponse " ++ ver ++ " " ++ code ++ " " ++ cmt

responseStatus : RawResponse String -> Maybe ResponseStatus
responseStatus (MkRawResponse r) = do
  status <- Prelude.Strings.split (== ' ') <$> (head' . lines $ r)

  -- hack, hack, hack
  ver <- head' status
  code <- tail' status >>= head'
  cmt <- tail' status >>= tail' >>= head'
  return (MkResponseStatus ver code cmt)
