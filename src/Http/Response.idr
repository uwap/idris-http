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

parseResponseStatus : RawResponse String -> Maybe ResponseStatus
parseResponseStatus (MkRawResponse r) with (lines r)
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
  | (k :: v :: []) = Just (k, trim v)
  | _ = Nothing
   
||| Parse a response message as defined in RFC7230 Section 3.
|||
||| @ rres A raw HTTP response
parseResponse : (rres : RawResponse String) -> Maybe Response
parseResponse (MkRawResponse str) with (lines str)
  | [] = Nothing
  | (x :: xs) = Just $ MkResponse !(parseResponseStatus (MkRawResponse x)) (fromList !(parseLines xs))
  where
    parseLines : List String -> Maybe (List (String, String))
    parseLines (x :: xs) = Just $ !(parseLines xs) ++ pure !(parseHeaderField x)
    parseLines ("" :: xs) = Just [] -- parseBody (unlines xs)
                    -- TODO: Implement parseBody
    parseLines [] = Just []
