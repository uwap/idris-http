module Http.RawResponse

import Classes.Verified

data RawResponse a = MkRawResponse a

instance Eq a => Eq (RawResponse a) where
  (MkRawResponse a) == (MkRawResponse b) = a == b
  (MkRawResponse a) /= (MkRawResponse b) = a /= b

instance Show a => Show (RawResponse a) where
  show (MkRawResponse a) = show a

instance Functor RawResponse where
  map f (MkRawResponse a) = MkRawResponse (f a)

instance Applicative RawResponse where
  pure = MkRawResponse
  (MkRawResponse f) <*> (MkRawResponse a) = MkRawResponse (f a)

instance Monad RawResponse where
  (MkRawResponse a) >>= f = f a

instance Foldable RawResponse where
  foldr f z (MkRawResponse a) = f a z

instance Traversable RawResponse where
  traverse f (MkRawResponse x) = [| MkRawResponse (f x) |]

instance Semigroup a => Semigroup (RawResponse a) where
  (MkRawResponse a) <+> (MkRawResponse b) = MkRawResponse (a <+> b)

instance Monoid a => Monoid (RawResponse a) where
  neutral = MkRawResponse neutral

instance Cast a (RawResponse a) where
  cast = pure
instance Cast (RawResponse a) a where
  cast (MkRawResponse a) = a

instance VerifiedFunctor RawResponse where
  functorIdentity (MkRawResponse _) = Refl
  functorComposition (MkRawResponse _) _ _ = Refl

instance VerifiedApplicative RawResponse where
  applicativeMap (MkRawResponse _) _ = Refl
  applicativeIdentity (MkRawResponse _) = Refl
  applicativeComposition (MkRawResponse _) (MkRawResponse _) (MkRawResponse _) =
    Refl
  applicativeHomomorphism _ _ = Refl
  applicativeInterchange _ (MkRawResponse _) = Refl

instance VerifiedMonad RawResponse where
  monadApplicative (MkRawResponse _) (MkRawResponse _) = Refl
  monadLeftIdentity _ _ = Refl
  monadRightIdentity (MkRawResponse _) = Refl
  monadAssociativity (MkRawResponse _) _ _ = Refl

instance VerifiedSemigroup a => VerifiedSemigroup (RawResponse a) where
  semigroupOpIsAssociative (MkRawResponse a) (MkRawResponse b) (MkRawResponse c) =
                                          rewrite semigroupOpIsAssociative a b c in Refl

instance VerifiedMonoid a => VerifiedMonoid (RawResponse a) where
  monoidNeutralIsNeutralL (MkRawResponse a) = rewrite monoidNeutralIsNeutralL a in Refl
  monoidNeutralIsNeutralR (MkRawResponse a) = rewrite monoidNeutralIsNeutralR a in Refl
