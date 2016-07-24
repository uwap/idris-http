module Http.Response

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

import Data.SortedMap
import Http.Request
import Http.RawResponse
import Http.Error

%access export

public export
record ResponseStatus where
  constructor MkResponseStatus
  responseStatusVersion : String
  responseStatusCode : Int
  responseStatusComment : String

implementation Show ResponseStatus where
  show (MkResponseStatus ver code cmt) =
    "MkResponseStatus " ++ ver ++ " " ++ show code ++ " " ++ cmt

public export
record Response a where
  constructor MkResponse
  responseStatus : ResponseStatus
  ||| A Map of all response headers with the key being all lowercase
  responseHeaders : SortedMap String String
  responseBody : a

getHeader : Response a -> String -> Maybe String
getHeader r k = lookup (toLower k) (responseHeaders r)

private
responseStatusParser : Parser ResponseStatus
responseStatusParser = do
  version <- some (noneOf " \n\r")
  space
  statusCode <- integer
  space
  comment <- some (noneOf "\n\r")
  crlf
  return $ MkResponseStatus (pack version) statusCode (pack comment)

private
headerFieldParser : Parser (String, String)
headerFieldParser = do
  key <- some (noneOf ":\n\r")
  char ':' >! opt (char ' ')
  value <- some (noneOf "\n\r")
  opt (char ' ') >! crlf
  return (pack (toLower <$> key), pack value)

bodyParser : SortedMap String String -> Parser String
bodyParser map with (lookup "content-length" map)
  | Nothing = return "error: Chunked encoding not supported yet."
  | Just x = pack <$> ntimes (cast $ the Int (cast x)) anyChar

private
responseParser : Parser (Response String)
responseParser = do
  status <- responseStatusParser
  fields <- many headerFieldParser
  body <- crlf >! bodyParser (fromList fields)
  return $ MkResponse status (fromList fields) body

parseResponse : RawResponse String -> Either HttpError (Response String)
parseResponse (MkRawResponse input) = case parse responseParser input of
                                           Left err => Left $ HttpParseError err
                                           Right re => Right re 
