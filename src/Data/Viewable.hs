{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module Data.Viewable (Viewable(..)) where

import Data.Kind (type Type)
import Data.Singletons (Sing)

class Viewable k where
  data View :: k -> Type
  view :: Sing (a :: k) -> View a
