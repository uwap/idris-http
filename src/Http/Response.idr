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

record Response where
  constructor MkResponse
  responseStatus : ResponseStatus
  responseHeaders : Vect n (String, String)
  responseBody : String

parseResponseStatus : String -> Maybe ResponseStatus
parseResponseStatus r with (lines r)
  | (x :: _) with (words x)
                | (_ :: _ :: []) = Nothing
                | (ver :: code :: cmt) with (the Int (cast code))
                  | 0 = Nothing
                  | n = Just $
                        MkResponseStatus ver n (unwords cmt)
                | _ = Nothing
  | [] = Nothing

||| Parses one header line as defined in RFC7230 Section 3.2.
parseHeaderField : String -> Maybe (String, String)
parseHeaderField line with (split (==':') line)
  | (x :: []) = Nothing
  | (k :: v) = Just (k, trim $ join ":" v)
  where
    join : String -> List String -> String
    join p [] = ""
    join p (x :: []) = x
    join p (x :: xs) = x ++ p ++ join p xs
  | _ = Nothing

||| Parse a response message as defined in RFC7230 Section 3.
|||
||| @ rres A raw HTTP response
parseResponse : (rres : RawResponse String) -> Maybe Response
parseResponse (MkRawResponse str) =
    let (rhead, rbody) = splitBody (unpack str) in
      case lines rhead of
        [] => Nothing
        (x :: xs) => parseLines (MkResponse !(parseResponseStatus x) [] rbody) xs
  where
    splitBody : List Char -> (String, String)
    splitBody ('\r' :: '\n' :: '\r' :: '\n' :: xs) = ("", pack xs)
    splitBody [] = ("", "")
    splitBody (x :: xs) = let (h, b) = splitBody xs in (strCons x h, b)

    unlines : List String -> String
    unlines [] = ""
    unlines (x :: xs) = x
    unlines (x :: y :: xs) = x ++ "\r\n" ++ unlines (y :: xs)

    parseLines : Response -> List String -> Maybe Response
    parseLines r (x :: xs) = do
      r' <- parseLines r xs
      Just $ record { responseHeaders = !(parseHeaderField x) :: responseHeaders r' } r'
    parseLines r [] = Just r
