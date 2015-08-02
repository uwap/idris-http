module Http.RawResponse

data RawResponse a = MkRawResponse a

instance Show a => Show (RawResponse a) where
  show (MkRawResponse a) = show a

instance Functor RawResponse where
  map f (MkRawResponse a) = MkRawResponse (f a)

instance Applicative RawResponse where
  pure = MkRawResponse
  (MkRawResponse f) <*> (MkRawResponse a) = MkRawResponse (f a)

instance Monad RawResponse where
  (MkRawResponse a) >>= f = f a
