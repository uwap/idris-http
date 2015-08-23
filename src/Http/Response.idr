module Http.Response

import Data.SortedMap
import Http.Request
import Http.RawResponse
import Http.Error

%access public

record ResponseStatus where
  constructor MkResponseStatus
  responseStatusVersion : String
  responseStatusCode : Int
  responseStatusComment : String

instance Show ResponseStatus where
  show (MkResponseStatus ver code cmt) =
    "MkResponseStatus " ++ ver ++ " " ++ show code ++ " " ++ cmt

record Response a where
  constructor MkResponse
  responseStatus : ResponseStatus
  responseHeaders : SortedMap String String
  responseBody : a

private
parseResponseStatus : String -> Either HttpError ResponseStatus
parseResponseStatus x with (words x)
  | (_ :: _ :: []) = Left $ HttpParseError "Error parsing Response Status Line."
  | (ver :: code :: cmt) with (the Int (cast code))
    | 0 = Left $ HttpParseError "Invalid Status Code."
    | n = Right $
          MkResponseStatus ver n (unwords cmt)
  | _ = Left $ HttpParseError "Error parsing Response Status Line."

||| Parses one header line as defined in RFC7230 Section 3.2.
private
parseHeaderField : String -> Either HttpError (String, String)
parseHeaderField line with (split (==':') line)
  | (x :: []) = Left $ HttpParseError "Error parsing Response Header."
  | (k :: v) = Right (k, trim $ join ":" v)
  where
    join : String -> List String -> String
    join p [] = ""
    join p (x :: []) = x
    join p (x :: xs) = x ++ p ++ join p xs
  | _ = Left $ HttpParseError "Error parsing Response Header."

||| Get the length of the response body as defined in RFC7230 Section 3.3.3.
||| INCOMPLETE
private
getResponseBodyLength : Request a -> Response b -> Integer
getResponseBodyLength req res = let code = responseStatusCode . responseStatus $ res in
  if method req == HEAD then 0 else                                        -- 1. Response to HEAD request
  if code == 204 || code == 304 || (code >= 100 && code < 200) then 0 else -- 1. Response Code 204, 304, 1xx
  if code >= 200 && code < 300 then 0 else                                 -- 2. Response Code 2xx
  0 -- TODO: Return the length defined in the responseHeader

||| Parse a response message as defined in RFC7230 Section 3.
||| INCOMPLETE due to:
|||   1. No call to getResponseBodyLength
|||   2. getResponseBodyLength is incomplete
|||   3. doesn't allow reading multiple Http responses at once.
|||   4. No encoding
|||   5. TODO: Add more reasons
|||
||| @ rres A raw HTTP response
parseResponse : (rres : RawResponse String) -> Either HttpError (Response String)
parseResponse (MkRawResponse str) =
    let (rhead, rbody) = splitBody (unpack str) in
      case lines rhead of
        [] => Left $ HttpParseError "The response was empty."
        (x :: xs) => parseLines (MkResponse !(parseResponseStatus x) empty rbody) xs
  where
    splitBody : List Char -> (String, String)
    splitBody ('\r' :: '\n' :: '\r' :: '\n' :: xs) = ("", pack xs)
    splitBody [] = ("", "")
    splitBody (x :: xs) = let (h, b) = splitBody xs in (strCons x h, b)

    unlines : List String -> String
    unlines [] = ""
    unlines (x :: xs) = x
    unlines (x :: y :: xs) = x ++ "\r\n" ++ unlines (y :: xs)

    parseLines : Response a -> List String -> Either HttpError (Response a)
    parseLines r (x :: xs) = do
      r' <- parseLines r xs
      Right $ record { responseHeaders = uncurry insert !(parseHeaderField x) (responseHeaders r') } r'
    parseLines r [] = Right r


