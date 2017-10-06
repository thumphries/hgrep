{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.HGrep.Prelude (
  -- * Primitive types
  -- ** Bool
    Bool (..)
  , bool
  , (&&)
  , (||)
  , not
  , otherwise
  -- ** Char
  , Char
  -- ** Int
  , Integer
  , Int
  , Int8
  , Int16
  , Int32
  , Int64
  -- ** Word
  , Word64
  -- ** Real
  , fromIntegral
  , fromRational

  -- * Algebraic structures
  -- ** Monoid
  , Monoid (..)
  , (<>)
  -- ** Functor
  , Functor (..)
  , (<$>)
  , ($>)
  , void
  , with
  -- ** Bifunctor
  , Bifunctor (..)
  -- ** Applicative
  , Applicative (..)
  , (<**>)
  -- ** Alternative
  , Alternative (..)
  , asum
  -- ** Monad
  , Monad (..)
  , join
  -- ** MonadPlus
  , MonadPlus (..)
  , guard
  , msum
  -- ** MonadIO
  , MonadIO (..)

  -- * Data structures
  -- ** Either
  , Either (..)
  , either
  , note
  -- ** Maybe
  , Maybe (..)
  , fromMaybe
  , maybe
  , hush
  -- ** Tuple
  , fst
  , snd
  , curry
  , uncurry


  -- * Typeclasses
  -- ** Enum
  , Enum (..)
  -- ** Eq
  , Eq (..)
  -- ** Read
  , Read (..)
  , readEither
  , readMaybe
  -- ** Show
  , Show (..)
  -- *** ShowS
  , ShowS
  , showString
  -- ** Foldable
  , Foldable (..)
  , for_
  , all
  -- ** Ord
  , Ord (..)
  , Ordering (..)
  , comparing
  -- ** Traversable
  , Traversable (..)
  , for
  , traverse_


  -- * Combinators
  , id
  , (.)
  , ($)
  , ($!)
  , (&)
  , const
  , flip
  , fix
  , on
  , seq

  -- * Predicates
  -- ** Char
  , isSpace

  -- * System
  -- ** IO
  , IO
  , FilePath

  -- * Partial functions
  , undefined
  , error

  -- * Debugging facilities
  , trace
  , traceM
  , traceIO
  ) where


import           Control.Monad as Monad (
           Monad (..)
         , MonadPlus (..)
         , guard
         , join
         , msum
         )
import           Control.Monad.IO.Class (
           MonadIO (..)
         )
import           Control.Applicative as Applicative (
           Applicative (..)
         , (<**>)
         , Alternative (..)
         , empty
         )

import           Data.Bifunctor as Bifunctor (
           Bifunctor (..)
         )
import           Data.Bool as Bool (
           Bool (..)
         , bool
         , (&&)
         , (||)
         , not
         , otherwise
         )
import           Data.Char as Char (
           Char
         , isSpace
         )
import           Data.Either as Either (
           Either (..)
         , either
         )
import           Data.Foldable as Foldable (
           Foldable (..)
         , asum
         , traverse_
         , for_
         , all
         )
import           Data.Function as Function (
           id
         , (.)
         , ($)
         , (&)
         , const
         , flip
         , fix
         , on
         )
import           Data.Functor as Functor (
           Functor (..)
         , (<$>)
         , ($>)
         , void
         )
import           Data.Eq as Eq (
           Eq (..)
         )
import           Data.Int as Int (
           Int
         , Int8
         , Int16
         , Int32
         , Int64
         )
import           Data.Maybe as Maybe (
           Maybe (..)
         , fromMaybe
         , maybe
         )
import           Data.Monoid as Monoid (
           Monoid (..)
         , (<>)
         )
import           Data.Ord as Ord (
           Ord (..)
         , Ordering (..)
         , comparing
         )
import           Data.Traversable as Traversable (
           Traversable (..)
         , for
         )
import           Data.Tuple as Tuple (
           fst
         , snd
         , curry
         , uncurry
         )
import           Data.Word as Word (
           Word64
         )

import qualified Debug.Trace as Trace

import           GHC.Real as Real (
           fromIntegral
         , fromRational
         )
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack (HasCallStack)
#endif

import           Prelude as Prelude (
           Enum (..)
         , Integer
         , seq
         , ($!)
         )
import qualified Prelude as Unsafe

import           System.IO as IO (
           FilePath
         , IO
         )

import           Text.Read as Read (
           Read (..)
         , readEither
         , readMaybe
         )
import           Text.Show as Show (
           Show (..)
         , ShowS
         , showString
         )


#if MIN_VERSION_base(4,9,0)
undefined :: HasCallStack => a
#else
undefined :: a
#endif
undefined =
  Unsafe.undefined
{-# WARNING undefined "'undefined' is unsafe" #-}

#if MIN_VERSION_base(4,9,0)
error :: HasCallStack => [Char] -> a
#else
error :: [Char] -> a
#endif
error =
  Unsafe.error
{-# WARNING error "'error' is unsafe" #-}

trace :: [Char] -> a -> a
trace =
  Trace.trace
{-# WARNING trace "'trace' should only be used while debugging" #-}

#if MIN_VERSION_base(4,9,0)
traceM :: Applicative f => [Char] -> f ()
#else
traceM :: Monad m => [Char] -> m ()
#endif
traceM =
  Trace.traceM
{-# WARNING traceM "'traceM' should only be used while debugging" #-}

traceIO :: [Char] -> IO ()
traceIO =
  Trace.traceIO
{-# WARNING traceIO "'traceIO' should only be used while debugging" #-}

with :: Functor f => f a -> (a -> b) -> f b
with =
  flip fmap
{-# INLINE with #-}

-- | Tag a 'Nothing'.
note :: a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b
{-# INLINEABLE note #-}

-- | Eliminate a 'Left'.
hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right b) = Just b
{-# INLINEABLE hush #-}
