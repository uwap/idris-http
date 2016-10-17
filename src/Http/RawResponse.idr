module Http.RawResponse

%access public export

data RawResponse a = MkRawResponse a

implementation Eq a => Eq (RawResponse a) where
  (MkRawResponse a) == (MkRawResponse b) = a == b
  (MkRawResponse a) /= (MkRawResponse b) = a /= b

implementation Show a => Show (RawResponse a) where
  show (MkRawResponse a) = show a

implementation Functor RawResponse where
  map f (MkRawResponse a) = MkRawResponse (f a)

implementation Applicative RawResponse where
  pure = MkRawResponse
  (MkRawResponse f) <*> (MkRawResponse a) = MkRawResponse (f a)

implementation Monad RawResponse where
  (MkRawResponse a) >>= f = f a

implementation Foldable RawResponse where
  foldr f z (MkRawResponse a) = f a z

implementation Traversable RawResponse where
  traverse f (MkRawResponse x) = [| MkRawResponse (f x) |]

implementation Semigroup a => Semigroup (RawResponse a) where
  (MkRawResponse a) <+> (MkRawResponse b) = MkRawResponse (a <+> b)

implementation Monoid a => Monoid (RawResponse a) where
  neutral = MkRawResponse neutral

implementation Cast a (RawResponse a) where
  cast = pure
implementation Cast (RawResponse a) a where
  cast (MkRawResponse a) = a
