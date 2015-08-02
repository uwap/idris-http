module Http.Response

import Data.Vect
import Http.RawResponse

%access public

record ResponseStatus where
  constructor MkResponseStatus
  responseStatusVersion : String
  responseStatusCode : Int
  responseStatusComment : String

instance Show ResponseStatus where
  show (MkResponseStatus ver code cmt) =
    "MkResponseStatus " ++ ver ++ " " ++ show code ++ " " ++ cmt

responseStatus : RawResponse String -> Maybe ResponseStatus
responseStatus (MkRawResponse r) with (lines r)
  | (x :: _) with (words x)
                | (_ :: _ :: []) = Nothing
                | (ver :: code :: cmt) with (the Int (cast code))
                  | 0 = Nothing
                  | n = Just $
                        MkResponseStatus ver n (unwords cmt)
                | _ = Nothing
  | [] = Nothing
