# Deriving in Haskell

## visualising automagically derived instances

If you are like me and need to see the actual code that gets generated
in order to understand a new concept, like deriving strategies, GHC has
quite a few flags for debugging the compiled code. Here is a list of
all https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#compiler-debugging-options
debugging flags, but we'll be using *-ddump-deriv* in particular, to
dump all derived instances Haskell generates automatically.

The way to use these flags is like this:

```
ghc [flags] haskell-file
```

so to see the derived instances, run:

```
ghc -ddump-deriv haskell-file
```

to make the output a bit more readable, use the following flags:

```
-dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes
```

# Deriving explained

Type classes are Haskell's way of doing _ad hoc_ polymorphism (or overloading),
which I won't explain in this exercise, better to have a look here
https://www.haskell.org/tutorial/classes.html if you're not familiar with the concept.

They allow us to declare which types are instances of which class, and to provide
definitions of the overloaded operations associated with a class.

For example, let's look at Eq class and the (==) operator.

```
class Eq a where
  (==) :: a -> a -> Bool
```

This may be read "a type _a_ is an instance of the class _Eq_ if there is
an (overloaded) operation ==, defined on it."

Instances for base types, like Bool, Char, Int etc, are already defined in the
base library. For custom data types we need to, either define instances
manually or use the _deriving_ mechanism, like this:

```
data Foo = Foo { bar :: String, baz :: Integer }

-- manually write the instance
instance Eq Foo where
  Foo br1 bz1 == Foo br2 bz2 = br1 == br2 && bz1 == bz2

-- use the deriving mechanism
data Bar = Bar { bbaz :: Integer } deriving Eq
```

# GHC deriving extensions

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-instances-of-extra-classes-data-etc

Haskell 98 allows derivation only of Eq, Ord, Enum, Ix, Bounded, Read,
and Show classes, but GHC extends this with:

## DeriveAnyClass

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-any-other-class

Allow use of any typeclass in deriving clauses.

```
{-# LANGUAGE DeriveAnyClass, DefaultSignatures #-}

class SPretty a where
  sPpr :: a -> String
  default sPpr :: Show a => a -> String
  sPpr = show

data Foo = Foo deriving (Show, SPretty)
```

## DeriveGeneric

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveGeneric

Allow automatic deriving of instances for the Generic typeclass.

```
{-# LANGUAGE DeriveGeneric #-}

data UserTree a = Node a (UserTree a) (UserTree a) | Leaf deriving Generic
```

## DeriveFunctor

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveFunctor

Allow automatic deriving of instances for the Functor typeclass.

```
{-# LANGUAGE DeriveFunctor #-}
data Example a = Ex a Char (Example a) (Example Char) deriving Functor
```

## DeriveDataTypeable

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-data-instances

Enable automatic deriving of instances for the Data typeclass

```
{-# LANGUAGE DeriveDataTypeable #-}

data ExampleTy a = ExTy a Char Int deriving Typeable
```

## DeriveFoldable

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-foldable-instances

Allow automatic deriving of instances for the Foldable typeclass.

```
{-# LANGUAGE DeriveFoldable #-}
data Example a = Ex a Char (Example a) (Example Char) deriving Foldable
```

## DeriveTraversable

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-traversable-instances

Allow automatic deriving of instances for the Traversable typeclass.


```
{-# LANGUAGE DeriveTraversable #-}
data Example a = Ex a Char (Example a) (Example Char) deriving Traversable
```


## StandaloneDeriving
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#stand-alone-deriving-declarations

```
{-# LANGUAGE StandaloneDeriving #-}

data Standalone a = Standalone { lone :: Maybe a }

deriving instance Eq a => Eq (Standalone a)
```

This extension allows us to use the standalone version of the deriving declaration.

Standalone deriving differs from standard deriving in:
* declaration does not need to be in the same module (opens up Orphan instances)
* instances can be more specific eg:
```
deriving instance Eq a => Eq (Standalone (Maybe a))
```
* and many more

## GeneralizedNewtypeDeriving
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-derived-instances-for-newtypes

```
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Dollars = Dollars Int deriving (Eq, Ord, Bounded)
```

If you want to inherit for example Num from Int, you can't, because
in Haskell 98 you can only inherit Eq, Ord, Enum, Bounded, Show
and Read (aka stock derivable classes) by deriving them.

This extension allows us to derive *Num* and other classes.

GHC will generate somethig that resembles 'instance Num Int => Num Dollars'

## EmptyDataDeriving

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-instances-for-empty-data-types

Allows deriving instances of standard type classes for empty data types.

```
{-# LANGUAGE EmptyDataDeriving #-}


-- empty data types have no constructor, only value is bottom.
data Empty deriving (Show)
```


# Deriving strategies
## Stock

## Newtype

## Via
