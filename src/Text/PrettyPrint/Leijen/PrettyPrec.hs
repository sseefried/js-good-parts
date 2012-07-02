{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.PrettyPrec
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Pretty class with precedence
----------------------------------------------------------------------

module Text.PrettyPrint.Leijen.PrettyPrec( 
  PrettyPrec(..)
) where

import Data.Ratio (Ratio)

import Text.PrettyPrint.Leijen

-- | Pretty printing with precedence.  A cross between 'Show' and 'Pretty'.
-- The 'prettyPrec' method defaults to discarding the context precedence
-- and invoking 'pretty'.  The reason 'PrettyPrec' derives from Pretty is
-- that so that this default is possible.
--
-- To make a 'Show' instance for a 'PrettyPrec' instance 'Foo', define
--
--   instance Show Foo where showsPrec p e = showsPrec p (prettyPrec p e)

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc
  prettyPrec = const pretty  -- default

instance PrettyPrec Doc
instance PrettyPrec ()
instance PrettyPrec Bool
instance PrettyPrec Char
instance PrettyPrec Int
instance PrettyPrec Integer
instance PrettyPrec Float
instance PrettyPrec Double

-- Orphan. Missing from wl-pprint
instance (Integral a, Show a) => Pretty (Ratio a) where
  pretty = text . show

instance Pretty a => PrettyPrec [a]

instance (Pretty a,Pretty b) => PrettyPrec (a,b)

instance (Pretty a,Pretty b,Pretty c) => PrettyPrec (a,b,c)

instance PrettyPrec a => PrettyPrec (Maybe a) where
  prettyPrec p = maybe empty (prettyPrec p)

instance (Integral a, Show a) => PrettyPrec (Ratio a) where
  prettyPrec = const (text . show)