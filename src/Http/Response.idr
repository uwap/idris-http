module Http.Response

import Data.Vect
import Http.RawResponse
import List.Split

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

partial   -- TODO: Nuke this annotation after https://github.com/relrod/idris-split/issues/1
splitDoubleCRLF : String -> (String, String)
splitDoubleCRLF s =
  case splitOn' "\r\n\r\n" s of
    (x :: [])      => (x, "")
    (x :: y :: []) => (x, y)
    (x :: y :: z)  => (x, y ++ concat (intersperse "\r\n" z))
    _              => ("", "")

partial   -- TODO: Nuke this annotation after https://github.com/relrod/idris-split/issues/1
splitColon : String -> (String, String)
splitColon s =
  case splitOn' ":" s of
    (x :: [])      => (x, "")
    (x :: y :: []) => (x, y)
    (x :: y :: z)  => (x, y ++ concat (intersperse ":" z))
    _              => ("", "")

-- TODO: uwap please make this better
partial   -- TODO: Nuke this annotation after https://github.com/relrod/idris-split/issues/1
responseHeaders : RawResponse String -> List (String, String)
responseHeaders (MkRawResponse r) =
  let headersRaw = fst $ splitDoubleCRLF r
      headerLines = drop 1 . lines $ headersRaw
      headers = map splitColon headerLines
  in headers

partial   -- TODO: Nuke this annotation after https://github.com/relrod/idris-split/issues/1
responseBody : RawResponse String -> String
responseBody (MkRawResponse r) = snd . splitDoubleCRLF $ r
