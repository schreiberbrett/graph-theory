TwoSet -- a Set which contains exactly two values.

Consider a set that can contain exactly two values.
\begin{code}
module TwoSet where

import qualified Data.Set as Set
import Data.Set (Set)

newtype TwoSet a = TwoSet' (Set a) deriving (Eq, Ord)

instance Show a => Show (TwoSet a) where
    show x = show $ unwrap x

wrap :: (Ord a) => a -> a -> TwoSet a
wrap x y = TwoSet' $ Set.insert y $ Set.singleton x

unwrap :: TwoSet a -> (a, a)
unwrap (TwoSet' x) = (l, r) where [l, r] = Set.toList x

\end{code}

Here we consider a set with only two values.

