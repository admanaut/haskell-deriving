# Deriving in Haskell

## visualising automagically derived instances

If you are like me and need to see the actual code that gets generated
in order to understand a new concept, like deriving strategies, GHC has
quite a few flags for debugging the compiled code. Here is a list of
all https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#compiler-debugging-options
debugging flags, but we'll be using *-ddump-deriv* in particular to
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
base library. For custom data types we need to either define instances
manually or use the _deriving_ mechanism, like this:

```
data Foo = Foo { bar :: String, baz :: Integer }

-- manually write the instance
instance Eq Foo where
  Foo br1 bz1 == Foo br2 bz2 = br1 == br2 && bz1 == bz2

-- use the deriving mechanism
data Bar = Bar { bbaz :: Integer } deriving Eq
```

# Deriving extensions

*StandaloneDeriving*
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#stand-alone-deriving-declarations

```
{-# LANGUAGE StandaloneDeriving #-}

data Standalone a = Standalone { lone :: Maybe a}

deriving instance Eq a => Eq (Standalone a)
```

*GeneralizedNewtypeDeriving*
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-derived-instances-for-newtypes

```
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Dollars = Dollars Int deriving (Eq, Ord, Bounded)

{-
We would like to inherit some instances from Int but in Haskell 98 we can only
inherit Eq, Ord, Enum and Bounded by deriving them.

If we want to derive Num we need to use this extension.

GHC will generate somethig that resembles 'instance Num Int => Num Dollars'
-}
```

# Deriving strategies

## Stock

## Newtype

## Via
