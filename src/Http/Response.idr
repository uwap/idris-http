module Http.Response

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

import public Data.SortedMap
import public Http.Request
import public Http.RawResponse
import public Http.Error

import Data.Bytes as B

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
  pure $ MkResponseStatus (pack version) statusCode (pack comment)

private
headerFieldParser : Parser (String, String)
headerFieldParser = do
  key <- some (noneOf ":\n\r")
  char ':' >! opt (char ' ')
  value <- some (noneOf "\n\r")
  opt (char ' ') >! crlf
  pure (pack (toLower <$> key), pack value)

private
parseBodyChunkEncoded : Parser String
parseBodyChunkEncoded = do
    x <- hexParser
    opt $ do
      spaces
      char ';'
      some (noneOf "\n\r")
    crlf
    if x == 0 then many (noneOf "\n\r") *> crlf *> pure ""
              else liftA2 (++) (parseBody x) parseBodyChunkEncoded
  where
    hexParser' : Int -> Parser Int
    hexParser' x = do
      c <- hexDigit
      hex2 <- opt $ hexParser' (x*16)
      let hex = ord $ toUpper c
      let num = if hex >= ord '0' && hex <= ord '9'
                   then hex - ord '0'
                   else 10 + hex - ord 'A'
      case hex2 of
           Just x => pure (x * num)
           Nothing => pure num
    hexParser : Parser Int
    hexParser = hexParser' 1
    parseBody : Int -> Parser String
    parseBody x = do
      n <- pack <$> many (oneOf "\n\r")
      s <- pack <$> some (noneOf "\n\r")
      let len = toIntNat (B.length (B.fromString (s ++ n)))
      if x - len < 0 then fail ("Somehow the String " ++ s ++ n ++ " is longer than " ++ show x)
      else if x - len == 0 then crlf *> pure s
                           else map (s ++) $ parseBody (x - len)

private
bodyParser : SortedMap String String -> Parser String
bodyParser map with (lookup "content-length" map)
  | Nothing = parseBodyChunkEncoded
  | Just x = pack <$> ntimes (cast $ the Int (cast x)) anyChar

private
responseParser : Parser (Response String)
responseParser = do
  status <- responseStatusParser
  fields <- many headerFieldParser
  body <- crlf >! bodyParser (fromList fields)
  pure $ MkResponse status (fromList fields) body

parseResponse : RawResponse String -> Either HttpError (Response String)
parseResponse (MkRawResponse input) = case parse responseParser input of
                                           Left err => Left $ HttpParseError err
                                           Right re => Right re
