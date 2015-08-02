module Http.RawResponse

import Classes.Verified

data RawResponse a = MkRawResponse a
---------------------------------------------
instance Show a => Show (RawResponse a) where
  show (MkRawResponse a) = show a
---------------------------------------------
instance Functor RawResponse where
  map f (MkRawResponse a) = MkRawResponse (f a)

instance Applicative RawResponse where
  pure = MkRawResponse
  (MkRawResponse f) <*> (MkRawResponse a) = MkRawResponse (f a)

instance Monad RawResponse where
  (MkRawResponse a) >>= f = f a
--------------------------------------------
instance Cast a (RawResponse a) where
  cast = pure
instance Cast (RawResponse a) a where
  cast (MkRawResponse a) = a
--------------------------------------------
instance VerifiedFunctor RawResponse where
  functorIdentity (MkRawResponse x) = Refl
  functorComposition (MkRawResponse x) f g = Refl

instance VerifiedApplicative RawResponse where
  applicativeMap (MkRawResponse x) g = Refl
  applicativeIdentity (MkRawResponse x) = Refl
  applicativeComposition (MkRawResponse x)
                      (MkRawResponse f) (MkRawResponse g) = Refl
  applicativeHomomorphism x f = Refl
  applicativeInterchange x (MkRawResponse f) = Refl

instance VerifiedMonad RawResponse where
  monadApplicative (MkRawResponse f) (MkRawResponse x) = Refl
  monadLeftIdentity x f = Refl
  monadRightIdentity (MkRawResponse x) = Refl
  monadAssociativity (MkRawResponse x) f g = Refl
