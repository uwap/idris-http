module Http.Response

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

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
  return (pack key, pack value)

private
responseParser : Parser (Response String)
responseParser = do
  status <- responseStatusParser
  fields <- many headerFieldParser
  body <- crlf >! many anyChar
  return $ MkResponse status (fromList fields) (pack body)

parseResponse : RawResponse String -> Either HttpError (Response String)
parseResponse (MkRawResponse input) = case parse responseParser input of
                                           Left err => Left $ HttpParseError err
                                           Right re => Right re 
