import Control.Comonad
import Control.Comonad.Store
import Data.Functor.Identity

-- Basically just the store comonad with a funky ComonadApply
data Cartesian n a = Cartesian (n -> a) n

instance Functor (Cartesian n) where
  fmap f ~(Cartesian g p) = Cartesian (f . g) p

instance Comonad (Cartesian n) where
  extract ~(Cartesian f p) = f p
  duplicate ~(Cartesian f p) = Cartesian (\p' -> Cartesian f p') p

-- Could probably be group but w/e
instance (Num n) => ComonadApply (Cartesian n) where
  ~(Cartesian ff pf) <@> ~(Cartesian fa pa) =
    let dp = pa - pf
    in Cartesian (\p -> ff p (fa (p + dp))) pf

instance ComonadStore n (Cartesian n) where
  pos (Cartesian _ p) = p
  peek p (Cartesian f _) = f p

differentiate :: (Fractional a) => a -> (a -> a) -> (a -> a)
differentiate h f = \x -> (f (x + h) - f (x - h))/(2 * h)

epsilonEq :: (Fractional a, Ord a) => a -> a -> a -> Bool
epsilonEq ep x y = (abs (x - y)) < ep

maxima :: (Fractional a, Ord a, Show a) => Cartesian a a -> Cartesian a a
maxima (Cartesian f p) =
  let epsilon = realToFrac 0.1
      delta = realToFrac 0.001
      f' = differentiate delta f
  in kfix $ Cartesian (\p' ->
                         let slope = f' p'
                         in if | (epsilonEq epsilon slope 0) -> const p'
                               | slope > 0 -> extract . seeks (\p -> p + delta)
                               | slope < 0 -> extract . seeks (\p -> p - delta)) p
