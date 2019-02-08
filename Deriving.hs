{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
Manually deriving instances
-}
data Foo = Foo { bar :: String, baz :: Integer }

instance Eq Foo where
  Foo br1 bz1 == Foo br2 bz2 = br1 == br2 && bz1 == bz2

{-
Deriving using @deriving@
-}
data Bar = Bar { bbaz :: Integer } deriving Eq

{-
StandaloneDeriving extension
-}
data Standalone a = Standalone { lone :: Maybe a}

deriving instance Eq a => Eq (Standalone a)

{-
GeneralizedNewtypeDeriving extension
-}
newtype Dollars = Dollars Int deriving (Show, Eq, Ord, Bounded, Num)
