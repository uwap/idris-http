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
responseStatus (MkRawResponse r) with (lines r)
  | (x :: _) with (split (== ' ') x)
                | (ver :: code :: cmt :: _) = Just $
                    MkResponseStatus ver code cmt
                | _ = Nothing
  | [] = Nothing
