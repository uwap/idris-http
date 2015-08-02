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

||| Idris doesn't seem to yet have a port of Haskell's split package :-(.
||| As such, doing infix-substring splits are a bit evil. Here we just hardcode
||| the case we're after (\r\n).
splitDoubleCRLF : String -> (String, String)
splitDoubleCRLF s = splitDoubleCRLF' (the (List Char) List.Nil) (unpack s)
  where
    splitDoubleCRLF' consumed ('\r'::'\n'::'\r'::'\n'::rest) = (pack consumed, pack rest)
    splitDoubleCRLF' consumed (l::rest) = splitDoubleCRLF' (consumed ++ [l]) rest
    splitDoubleCRLF' consumed [] = (pack consumed, "")

-- TODO: uwap please make this better
responseHeaders : RawResponse String -> List (List String)
responseHeaders (MkRawResponse r) =
  let headersRaw = fst $ splitDoubleCRLF r
      headerLines = lines headersRaw
      headers = map (map trim . split (== ':')) headerLines
  in headers

responseBody : RawResponse String -> String
responseBody (MkRawResponse r) = snd . splitDoubleCRLF $ r
