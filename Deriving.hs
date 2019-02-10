import Data.Typeable
import GHC.Generics
import Numeric

{-
Manually deriving instances
-}
data Foo = Foo { bar :: String, baz :: Integer }

instance Eq Foo where
  Foo br1 bz1 == Foo br2 bz2 = br1 == br2 && bz1 == bz2

{-
Deriving using @deriving@

- GHC writes the instance for you
-}
data Bar = Bar { bbaz :: Integer } deriving Eq

-- ----------------------- -
-- GHC deriving extensions -
-- ----------------------- -

{- @DeriveAnyClass@ extension -}

{-# LANGUAGE DeriveAnyClass, DefaultSignatures #-}

class SPretty a where
  sPpr :: a -> String
  default sPpr :: Show a => a -> String
  sPpr = show

data Foo = Foo deriving (Show, SPretty)

{-
instance Show Foo where
  showsPrec _ Foo = showString "Foo"

instance SPretty Foo where
-}

{- @DeriveFunctor@ extension -}

{-# LANGUAGE DeriveFunctor #-}
data Example a = Ex a Char (Example a) (Example Char) deriving Functor

{-
instance Functor Example where
  fmap f (Ex a1 a2 a3 a4)
    = Ex (f a1) a2 (fmap f a3) a4
  (<$) z (Ex a1 a2 a3 a4)
    = Ex z a2 ((<$) z a3) a4

-}


{- @DeriveFoldable@ extension -}

{-# LANGUAGE DeriveFoldable #-}
data ExampleFold a = ExFold a Char (ExampleFold a) (ExampleFold Char) deriving Foldable

{-
instance Foldable ExampleFold where
  foldr f z ExFold a1 a2 a3 a4
    = f a1 ((\ b1 b2 -> foldr f b2 b1) a3 z)
  foldMap f ExFold a1 a2 a3 a4 = mappend (f a1) (foldMap f a3)
  null (ExFold _ _ _ _) = False
-}


{- @DeriveTraversable@ extension -}

{-# LANGUAGE DeriveTraversable #-}
data ExampleTr a = ExTr a Char (ExampleTr a) (ExampleTr Char)
  deriving (Functor, Foldable, Traversable)

{-
instance Traversable ExampleTr where
  traverse f ExTr a1 a2 a3 a4
    = liftA2 (\ b1 b3 -> ExTr b1 a2 b3 a4) (f a1) (traverse f a3)
-}


{- @DeriveDataTypeable@ extension -}

{-# LANGUAGE DeriveDataTypeable #-}

data ExampleTy a = ExTy a Char Int deriving Typeable


{- @DeriveGeneric@ extension -}

{-# LANGUAGE DeriveGeneric #-}

data UserTree a = Node a (UserTree a) (UserTree a) | Leaf deriving Generic

{-
  instance Generic (UserTree a) where
    from x
      = M1
          (case x of
             Node g1 g2 g3
               -> L1 (M1 ((:*:) (M1 (K1 g1)) ((:*:) (M1 (K1 g2)) (M1 (K1 g3)))))
             Leaf -> R1 (M1 U1))
    to (M1 x)
      = case x of
          (L1 (M1 ((:*:) (M1 (K1 g1)) ((:*:) (M1 (K1 g2)) (M1 (K1 g3))))))
            -> Node g1 g2 g3
          (R1 (M1 U1)) -> Leaf


Derived type family instances:
  type Rep (UserTree a) = D1
                            ('MetaData "UserTree" "Main" "main" 'False)
                            (C1
                               ('MetaCons "Node" 'PrefixI 'False)
                               (S1
                                  ('MetaSel
                                     'Nothing
                                     'NoSourceUnpackedness
                                     'NoSourceStrictness
                                     'DecidedLazy)
                                  (Rec0 a)
                                :*: (S1
                                       ('MetaSel
                                          'Nothing
                                          'NoSourceUnpackedness
                                          'NoSourceStrictness
                                          'DecidedLazy)
                                       (Rec0 (UserTree a))
                                     :*: S1
                                           ('MetaSel
                                              'Nothing
                                              'NoSourceUnpackedness
                                              'NoSourceStrictness
                                              'DecidedLazy)
                                           (Rec0 (UserTree a))))
                             :+: C1 ('MetaCons "Leaf" 'PrefixI 'False) U1)
-}

{-
@StandaloneDeriving@ extension
- can be in a different module
- more specific
-}

{-# LANGUAGE StandaloneDeriving #-}

data Standalone a = Standalone { lone :: Maybe a}

deriving instance Eq a => Eq (Standalone a)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
GeneralizedNewtypeDeriving extension

- derives non base classes, like Num
-}

newtype Dollars = Dollars Int deriving (Show, Eq, Ord, Bounded, Num)

{-
  instance Num Dollars where
    (+)
      = coerce @(Int -> Int -> Int) @(Dollars -> Dollars -> Dollars) (+)
    (-)
      = coerce @(Int -> Int -> Int) @(Dollars -> Dollars -> Dollars) (-)
    (*)
      = coerce @(Int -> Int -> Int) @(Dollars -> Dollars -> Dollars) (*)
    negate = coerce @(Int -> Int) @(Dollars -> Dollars) negate
    abs = coerce @(Int -> Int) @(Dollars -> Dollars) abs
    signum = coerce @(Int -> Int) @(Dollars -> Dollars) signum
    fromInteger
      = coerce @(Integer -> Int) @(Integer -> Dollars) fromInteger
-}

-- equivalent to
-- instance Num Int => Num Dollars

{-# LANGUAGE EmptyDataDeriving #-}
{-
EmptyDataDeriving extension

- derives standard class instances for empty data types

-}

data Empty deriving (Show)

{-
instance Show Empty where
  showsPrec _ z = case z of
-}

{-# LANGUAGE DerivingVia, DerivingStrategies #-}

newtype Hex a = Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype Unicode = U Int
  deriving Show
    via (Hex Int)

-- >>> euroSign
-- 0x20ac
euroSign :: Unicode
euroSign = U 0x20ac

-- Generates the following instance

{-
instance Show Unicode where
  show :: Unicode -> String
  show = Data.Coerce.coerce
    @(Hex Int -> String)
    @(Unicode -> String)
    show
-}


newtype Unicode = U Int
  deriving Num
    via Int

  deriving Show
    via (Hex Int)

euroSign :: Unicode
euroSign = 0x20ac
