{-# LANGUAGE RecordWildCards #-}

module AlexPosn
  ( AlexPosn(AlexPosn)
  , Locatable(Locatable, locatableValue, locatablePosns)
  , pushLocatable
  ) where

import Control.Applicative (liftA2)
import Control.Arrow ((***))

-- | Combine two binary functions to give a binary function on pairs
(****) :: (a -> b -> c)    -- ^ A function \(f\)
       -> (a' -> b' -> c') -- ^ A function \(g\)
       -> (a, a')          -- ^ Values \((x_1, y_1)\)
       -> (b, b')          -- ^ Values \((x_2, y_2)\)
       -> (c, c')          -- ^ \((f(x_1, x_2), g(x_1, x_2))\)
f **** g = uncurry (***) . (f *** g)

-- | `AlexPosn' records the location of a token in the input text.  It has three
-- fields: @address@ (number of characters preceding the token), @line@
-- and @column@ of a token within the file.
data AlexPosn = AlexPosn !Int !Int !Int -- ^ Construct an `AlexPosn' with @address@, @line@ and @column@
  deriving (Eq, Ord, Show)

-- | A wrapper for values with a corresponding location in the input stream
data Locatable a = Locatable
  { locatableValue :: a                          -- ^ The value
  , locatablePosns :: Maybe (AlexPosn, AlexPosn) -- ^ Possibly the corresponding start and end positions in the input stream
  }
  deriving Show

instance Functor Locatable where
  fmap f l = l { locatableValue = f . locatableValue $ l }

-- | This has the new location spanning the locations of the inputs
instance Applicative Locatable where
  pure x = Locatable x Nothing
  (Locatable f ps1)     <*> (Locatable x Nothing) = Locatable (f x) ps1
  (Locatable f Nothing) <*> (Locatable x ps2)     = Locatable (f x) ps2
  (Locatable f ps1)     <*> (Locatable x ps2)     = Locatable (f x) (liftA2 (min **** max) ps1 ps2)

pushLocatable :: Functor m => Locatable (m a) -> m (Locatable a)
pushLocatable Locatable{..} = (flip Locatable locatablePosns) <$> locatableValue

