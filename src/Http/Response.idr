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
  | (x :: _) with (words x)
                | (_ :: _ :: []) = Nothing
                | (ver :: code :: cmt) = Just $
                    MkResponseStatus ver code (unwords cmt)
                | _ = Nothing
  | [] = Nothing
