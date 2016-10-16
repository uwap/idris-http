module Http.RawResponse

import Interfaces.Verified

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

implementation VerifiedFunctor RawResponse where
  functorIdentity (MkRawResponse _) = Refl
  functorComposition (MkRawResponse _) _ _ = Refl

implementation VerifiedApplicative RawResponse where
  applicativeMap (MkRawResponse _) _ = Refl
  applicativeIdentity (MkRawResponse _) = Refl
  applicativeComposition (MkRawResponse _) (MkRawResponse _) (MkRawResponse _) =
    Refl
  applicativeHomomorphism _ _ = Refl
  applicativeInterchange _ (MkRawResponse _) = Refl

implementation VerifiedMonad RawResponse where
  monadApplicative (MkRawResponse _) (MkRawResponse _) = Refl
  monadLeftIdentity _ _ = Refl
  monadRightIdentity (MkRawResponse _) = Refl
  monadAssociativity (MkRawResponse _) _ _ = Refl

implementation VerifiedSemigroup a => VerifiedSemigroup (RawResponse a) where
  semigroupOpIsAssociative (MkRawResponse a) (MkRawResponse b) (MkRawResponse c) =
                                          rewrite semigroupOpIsAssociative a b c in Refl

implementation VerifiedMonoid a => VerifiedMonoid (RawResponse a) where
  monoidNeutralIsNeutralL (MkRawResponse a) = rewrite monoidNeutralIsNeutralL a in Refl
  monoidNeutralIsNeutralR (MkRawResponse a) = rewrite monoidNeutralIsNeutralR a in Refl
