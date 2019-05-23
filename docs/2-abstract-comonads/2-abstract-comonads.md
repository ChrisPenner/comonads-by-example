build-lists: true
theme: Ostrich, 3
slide-transition: true

^ background-color: #0F0E0E
^ text: #FF5481
^ header: #FF5481
^ text-emphasis: #FFFFFF
^ text-strong: #FF5481
^ code: auto(25)

#[fit] **Abstract Comonads**
#### *for when comonads aren't abstract enough*

---

# REVIEW

## TODO add more review here once presentations are finalized

```haskell
class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)

  extend :: (w a -> b) -> w a -> w b
```

---

```haskell
λ> countStream
1 :> 2 :> 3 :> 4 :> 5 :> ...

λ> extract countStream
1
```

---
# Notation (extend)

```haskell
λ> extend (ix 2) countStream
3 :> 4 :> 5 :> 6 :> 7 :> ...

λ> countStream =>> ix 2
3 :> 4 :> 5 :> 6 :> 7 :> ...
```

---
# Notation (extend)

[.code-highlight: 1-2]
[.code-highlight: 1-5]
[.code-highlight: 1-8]
[.code-highlight: all]
```haskell
(=>>) :: w a -> (w a -> b) -> w b

λ> countStream
1 :> 2 :> 3 :> 4 :> 5 :> ...

λ> countStream =>> ix 2
3 :> 4 :> 5 :> 6 :> 7 :> ...

λ> countStream =>> ix 2 =>> takeS 3
[3,4,5] :> [4,5,6] :> [5,6,7] :> [6,7,8] :> [7,8,9] :> ...
```

---
# Notation (extend)

```haskell
λ> extract $ countStream =>> ix 2 =>> takeS 3
[3,4,5]
```

---

# Notation (co-kleisli composition)

```haskell
(=>=) :: (w a -> b) -> (w b -> c) -> w a -> c

λ> ix 2 =>= takeS 3 $ countStream
[3,4,5]
```

---


# Comonads 
## As Abstract Spaces

---

# Identity

```haskell
data Identity a = Identity a

instance Comonad Identity where
extract :: Identity a -> a
extract   (Identity a) = ???
```

---

# Identity
```haskell
extract   (Identity a) = a
```
---

# Identity
```haskell
duplicate (Identity a) = ???
```
---

# Identity
```haskell
duplicate (Identity a) = Identity (Identity a)
```
---

# Identity
```haskell
extend :: (Identity a -> b) 
       -> Identity a 
       -> Identity b
extend f  (Identity a) = ???
```

---

# Identity
```haskell
extend :: (Identity a -> b) 
       -> Identity a 
       -> Identity b
extend f  (Identity a) = Identity (f (Identity a))
```

---

# Env a.k.a. Co-Reader 
## `a.k.a. (e, a)`

```haskell
data Env e a = Env e a
    deriving (Eq, Show, Functor)
```

---
# Env a.k.a. Co-Reader 

```haskell
instance Comonad (Env e) where
extract :: Env e a -> a
extract   (Env _ a) = ???
```

---
# Env a.k.a. Co-Reader 

```haskell
instance Comonad (Env e) where
extract :: Env e a -> a
extract   (Env _ a) = a
```

---
# Env a.k.a. Co-Reader 

```haskell
duplicate :: Env e a -> Env e (Env e a)
duplicate (Env e a) = ???
```

---
# Env a.k.a. Co-Reader 

```haskell
duplicate :: Env e a -> Env e (Env e a)
duplicate (Env e a) = Env e (Env e a)
```

---
# Env a.k.a. Co-Reader 

```haskell
extend :: (Env e a -> b) 
       -> Env e a 
       -> Env e b
extend f  (Env e a) = ???
```

---
# Env a.k.a. Co-Reader 

```haskell
extend :: (Env e a -> b) 
       -> Env e a 
       -> Env e b
extend f  (Env e a) = Env e (f (Env e a))
```

---
# Env a.k.a. Co-Reader 


[.code-highlight: 1-3]
[.code-highlight: 4-5]
[.code-highlight: all]
```haskell
ask :: Env e a -> e
ask (Env e _) = e

asks :: (e -> e') -> Env e a -> e'
asks f (Env e _) = f e
```

---
# Env a.k.a. Co-Reader 

```haskell
local :: (e -> e') -> Env e a -> Env e' a
local f (Env e a) = Env (f e) a
```

---

# Env Intuition

| **extract:** | Get the value | 
| ---: | :--- |
| **extend:** | Use the environment AND value in a computation |
| **ask:** | What's my context? |
| **local:** | Adjust the context |

---

# Env Example

```haskell
vowels :: String
vowels = "aAeEiIoOuU"

filtered :: Env String String -> String
filtered w = let invalid = ask w
              in filter (not . (`elem` invalid)) $ extract w

append :: String -> Env e String -> String              
append s w = extract w ++ s

extract $ env vowels "Hello World" =>> filtered =>> append "!!"
--- > Hll Wrld!!
extract $ env vowels "Hello World" =>> filtered . local (++ ['A'..'Z'])
--- > ll rld
```

---

# Env Example

```haskell
type Range = (Int, Int)

clamp :: Env Range Int -> Int
clamp w = 
    let (lowest, highest) = ask w
    in max lowest . min highest . extract $ w

λ> clamp (Env (0, 10) 15)
10
λ> extend clamp (Env (0, 10) 15)
Env (0,10) 10
```

---

# Env Example

```haskell
moveBy :: Int -> Env Range Int -> Int
moveBy n = clamp . fmap (+n)

moveTo :: Int -> Env Range Int -> Int
moveTo n = clamp . fmap (const n)

adjustUpper :: Int -> Env Range Int -> Env Range Int
adjustUpper n = local (second (+n))

adjustLower :: Int -> Env Range Int -> Env Range Int
adjustLower n = local (first (+n))
```

---

[.code-highlight: 1-3]
[.code-highlight: 1-6]
[.code-highlight: 1-9]
[.code-highlight: 1-12]
[.code-highlight: all]

```haskell
λ> x = Env (0, 5) 3
Env (0,5) 3

λ> x =>> moveBy 1
Env (0,5) 4

λ> x =>> moveBy 5
Env (0,5) 5

λ> x =>> moveTo 30
Env (0,5) 5

λ> adjustUpperBy 10 x =>> moveBy 5
Env (0,15) 8

```

---

# Store a.k.a. Co-State 
## `a.k.a (s, s -> a)`

```haskell
data Store s a = Store (s -> a) s
    deriving Functor
```

---

# Store

```haskell
instance Comonad (Store s) where
extract :: Store s a -> a
extract (Store f s) = f s
```

---
# Store

```haskell
duplicate :: Store s a -> Store s (Store s a)
duplicate (Store f s) =
    Store (\s' -> Store f s') s
```

---
# Store

[.code-highlight: 1-3]
[.code-highlight: 4-6]
[.code-highlight: 7-8]
[.code-highlight: all]

```haskell
pos :: Store s a -> s
pos (Store _ s) = s

peek :: s -> Store s a -> a
peek s (Store f _) = f s

peeks :: (s -> s) -> Store s a -> a
peeks g (Store f s) = f (g s)
```

---

# Store

```haskell
squared :: Store Int Int
squared = Store (\x -> x^2) 10

λ> pos squared
10
λ> extract squared
100 -- 10^2
λ> peek 2 squared
4 -- 2^2
λ> peeks (+2) squared
144 -- (10 + 2)^2
```

---

# Store

```haskell
seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s) = Store f (g s)

λ> extract $ seek 5 squared
25 -- 5^2
λ> extract $ seeks (+5) squared
225 -- (10 + 5)^2
```


---

# Store Intuition

| **extract:** | Get the value stored at the current key   | 
| ---: | :--- |
| **extend:** | Shift values (peek) or combine related values |
| **pos:** | Where am I? |
| **peek:** | What's at this key? |
| **seek:** | Move to another key |

---

# Store Example: Dictionary

```haskell
populations :: Map String Int
populations =
    fromList [ ("Canada",        37279811)
               , ("Poland",        38028278)
               , ("France",        65480710)
               , ("United States", 329093110)
               , ("Germany",       82438639)
               ]

λ> lookup "Canada" populations
Just 37279811

λ> lookup "Wakanda" populations
Nothing

```

---

```haskell
countryPopulation :: Store String (Maybe Int)
countryPopulation 
  = Store (\country -> lookup country populations) "Canada"
```

```haskell
λ> pos countryPopulation
"Canada"

λ> peek "Poland" countryPopulation
Just 38028278
```

---

# Store Example: Dictionary

[.code-highlight: 1]
[.code-highlight: 1-4]
[.code-highlight: 1-7]
[.code-highlight: all]
```haskell
λ> popDefault = fmap (fromMaybe 0) countryPopulation

λ> :t popDefault
popDefault :: Store String Int

λ> extract x
37279811

λ> peek "Wakanda" x
0
```

---
# Store Example: Dictionary

[.code-highlight: 1-3]
[.code-highlight: all]

```haskell
experiment :: Functor f 
           => (s -> f s) -> Store s a -> f a
experiment search (Store f s) = f <$> search s

λ> experiment (const ["Canada", "Poland", "Germany"]) popDefault
[37279811, 38028278, 82438639]
```

---
# Store Example: Squared

```haskell
λ> experiment (\n -> [n - 10, n + 10, n + 20, n + 30]) squared
[ 0    -- (10-10)^2
, 400  -- (10+10)^2
, 900  -- (10+20)^2
, 1600 -- (10+30)^2
]

λ> experiment (\n -> (n, n)) squared
(10,100)
```

---
# Store Example: Squared

[.code-highlight: 1-3]
[.code-highlight: 1-5]
[.code-highlight: 1-8]
[.code-highlight: all]

```haskell
λ> extract squared
100

withN :: Store Int (String, Int)
withN = extend (experiment (\n -> (show n, n))) squared

λ> extract withN
("10",100)

λ> peek 5 withN
("5",25)
```

---

# Store Example: Squared

[.code-highlight: 1-3]
[.code-highlight: 1-5]
[.code-highlight: all]

```haskell
shifted :: Store Int (String, Int)
shifted = extend (peeks (+10)) withN

λ> extract shifted
("20",400)

λ> peek 5 shifted
("15",225)
```
---

# Conway's Game of Life

![fit](./images/conway/pulsar.gif)

---

# Rules

- LIVING cells with 2 or 3 living neighbours stays alive
- DEAD cells with 3 living neighbours come to life
- All other scenarios cause a cell to die

---

github.com/alella/GOL

![fit](./images/conway/rules.gif)

---

![fit](./images/conway/rules.gif)

---

![fit](./images/conway/glider.gif)

---

![fit](./images/conway/glider-gun.gif)

---

```haskell
λ> animateGrid startingGrid
..#....|.#.....|..#....|.......|.......
#.#....|..##...|...#...|.#.#...|...#...
.##....|.##....|.###...|..##...|.#.#...
.......|.......|.......|..#....|..##...
.......|.......|.......|.......|.......
```

---

[.code-highlight: 1-2]
[.code-highlight: 1-5]
[.code-highlight: 6-8]
[.code-highlight: 9-13]
[.code-highlight: all]

```haskell
type Coord = (Sum Int, Sum Int)
type Grid = Store Coord Bool

startingGrid :: Grid
startingGrid = store checkAlive (0, 0)
  where
    checkAlive :: Coord -> Bool
    checkAlive coord = S.member coord livingCells

    livingCells :: S.Set Coord
    livingCells = S.fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
```

---

```haskell
λ> peek (0, 0) startingGrid
False

λ> peek (1, 0) startingGrid
True

λ> putStrLn $ drawGrid 3 startingGrid
..#
#.#
.##

λ> putStrLn . drawGrid 3 $ step startingGrid
.#.
..#
.##
```

---

```haskell
computeCellLiveness :: Store (Sum Int, Sum Int) Bool 
                    -> Bool

step :: Store (Sum Int, Sum Int) Bool
     -> Store (Sum Int, Sum Int) Bool 
```

---

# [fit] Live Coding
### _what could_ **possibly** _go wrong???_

---

[.code-highlight: 1-6]
[.code-highlight: all]

```haskell
computeCellLiveness :: Grid -> Bool
computeCellLiveness grid =
    case (currentCellAlive, numLivingNeighbours) of
        (True, 2) -> True
        (_,    3) -> True
        _         -> False
  where
    currentCellAlive :: Bool
    currentCellAlive = ???

    neighboursAlive :: Neighbours Bool
    neighboursAlive = ???

    numLivingNeighbours :: Int
    numLivingNeighbours = ???
```

---

```haskell
currentCellAlive :: Store (Sum Int, Sum Int) Bool -> Bool
currentCellAlive grid = ???
```

---

```haskell
currentCellAlive :: Store (Sum Int, Sum Int) Bool -> Bool
currentCellAlive grid = extract grid
```

---

```haskell
numLivingNeighbours :: Int
numLivingNeighbours = ???
```

---

```haskell
numLivingNeighbours :: Int
numLivingNeighbours = length . filter id . toList $ neighboursAlive
```

---

```haskell
data Neighbours a =
    Neighbours
      a  a  a
      a     a
      a  a  a
    deriving (Show, Eq, Functor, Foldable, Traversable)
```

---

```haskell
-- | Given a coordinate, compute all the neighbours of that position.
neighbourLocations :: Coord -> Neighbours Coord
neighbourLocations s = mappend s <$> Neighbours
   (-1, -1) (0, -1) (1, -1)
   (-1,  0)         (1,  0)
   (-1,  1) (0,  1) (1,  1)
```

---

```haskell
neighboursAlive :: Neighbours Bool
neighboursAlive = experiment neighbourLocations grid
```

---

```haskell
computeCellLiveness :: Grid -> Bool
computeCellLiveness grid =
    case (currentCellAlive, numLivingNeighbours) of
        (True, 2) -> True
        (_,    3) -> True
        _         -> False
  where
    currentCellAlive :: Bool
    currentCellAlive = extract grid
    neighboursAlive :: Neighbours Bool
    neighboursAlive = experiment neighbourLocations grid
    numLivingNeighbours :: Int
    numLivingNeighbours = length . filter id . toList $ neighboursAlive
```
---

# Traced a.k.a. Co-Writer
## `a.k.a. Monoid m => m -> a`

```haskell
newtype Traced m a = Traced (m -> a)
    deriving Functor
```

---
# Traced

```haskell
instance (Monoid m) => Comonad (Traced m) where
extract :: Traced m a -> a
extract (Traced f) = f mempty
```

---
# Traced

```haskell
duplicate :: Traced m a 
          -> Traced m (Traced m a)
duplicate (Traced f) =
    Traced $ \m -> Traced (f . mappend m)
```

---
# Traced

```haskell
extend :: (Traced m a -> b) 
       -> Traced m a 
       -> Traced m b
extend g = fmap g . duplicate
```

---

# Traced

```haskell
trace :: m -> Traced m a -> a
trace m (Traced f) = f m

times10 :: Traced (Sum Int) Int
times10 = traced (\(Sum n) -> n * 10 )

λ> extract times10
0
λ> trace (Sum 5) times10
50
λ> extract $ times10 =>> trace (Sum 1) =>> trace (Sum 2)
30
```

---

# EQUATIONAL REASONING

```haskell
> trace "hi" (Traced id)

> id "hi"

> "hi"
```

---


```haskell

> Traced id =>> trace "hi"

> trace "hi" <$> (duplicate $ Traced id) 

> trace "hi" <$> (Traced $ \m -> Traced (id . mappend m)) 

> Traced $ \m -> trace "hi" (Traced (id . mappend m))

> Traced $ \m -> (id . mappend m) "hi" 

> Traced $ \m -> (mappend m "hi")
```

---

```haskell
> extract $ Traced id =>> trace "hi" =>> trace "!!"

> extract $ Traced (\m -> (mappend m "hi")) =>> trace "!!"

> trace "!!" (Traced $ \m -> (mappend m "hi"))

> (\m -> (mappend m "hi")) "!!" 

> (mappend "!!" "hi") 

> "!!hi"
```

---

# Traced Example

```haskell
exclamation :: Traced String String
exclamation = traced (\s -> toUpper <$> s <> "!!")

λ> trace "hello" exclamation
"HELLO!!"
λ> extract $ exclamation =>> trace "hello"
"HELLO!!"
λ> extract $ exclamation =>> trace "jerry" =>> trace " " =>> trace "hello"
"HELLO JERRY!!"
```

---

```haskell
traces :: Monoid m => (a -> m) -> Traced m a -> a
traces f t = trace (f (extract t)) t

```

---

# Traced Intuition

| **extract:** | Run the computation at my current location | 
| ---: | :--- |
| **extend:** | move to another location (relative) |
| **trace** | What value is at this place near me? |
| **traces** | Given the value at my location; decide which nearby value to look at |

---

# Example: Function Derivative

![fit](./images/derivative/root-16.png)

---

$$
x^2 - 16
$$

![fit](./images/derivative/root-16.png)

---

![fit](./images/derivative/root-16.png)

---


```haskell
rootSolver :: Double -> Traced (Sum Double) Double
rootSolver n = Traced f
  where
    f :: Sum Double -> Double
    f (Sum x) = (x^2) - n
```

---

$$
x = 2 
$$

![fit](./images/derivative/derivative-point.png)

---

![fit](./images/derivative/derivative-point.png)

---

![fit](./images/derivative/derivative-context.png)

---

![fit](./images/derivative/derivative.png)

---

![fit](./images/derivative/derivative-plot.png)

---

![ fill](./images/derivative/derivative-plot.png)

![ fill](./images/derivative/derivative.png)

---

# Live Coding?

---

```haskell
solveRoot16 :: Double -> Double
solveRoot16 x = (x ^ (2 :: Integer)) - 16

solveRoot16T :: Traced (Sum Double) Double
solveRoot16T  = traced (solveRoot16 . getSum)
```

---

```haskell
estimateDerivativeAtPosition :: Traced (Sum Double) Double
                             -> Double
estimateDerivativeAtPosition w =
    let leftY = trace (Sum (-1)) w
        rightY = trace (Sum 1) w
        in (rightY - leftY) / 2
```

---

```haskell
estimateDerivativeAtPositionReader :: Traced (Sum Double) Double
                             -> Double
estimateDerivativeAtPositionReader = do
    leftY <- trace (Sum (-1))
    rightY <- trace (Sum 1)
    return $ (rightY - leftY) / 2
```

---

```haskell
estimateDerivative :: Traced (Sum Double) Double
                   -> Traced (Sum Double) Double
estimateDerivative = extend estimateDerivativeAtPosition
```

---

```haskell
withDerivative :: Traced (Sum Double) (Double, Double)
withDerivative = liftW2 (,) solveRoot16T (estimateDerivative solveRoot16T)
```

---

```haskell
λ> trace (Sum 1) withDerivative
(-15.0,2.0)
λ> trace (Sum 0) withDerivative
(-16.0,0.0)
λ> trace (Sum 4) withDerivative
(0.0,8.0)
```

---


# Example: Dependency Tracking

```haskell
ingredientsOf :: String -> S.Set String
ingredientsOf "string"  = S.fromList ["wool"]
ingredientsOf "sticks"  = S.fromList ["wood"]
ingredientsOf "bow"     = S.fromList ["sticks", "string"]
ingredientsOf "arrow"   = S.fromList ["sticks", "feather", "stone"]
ingredientsOf "quiver"  = S.fromList ["arrow", "bow"]
ingredientsOf "torches" = S.fromList ["coal", "sticks"]
ingredientsOf _         = mempty

recipes :: Traced (S.Set String) (S.Set String)
recipes = traced (foldMap ingredientsOf)
```

---

```haskell
string  -> wool
sticks  -> wood
bow     -> sticks, string
arrow   -> sticks, feather, stone
quiver  -> arrow, bow
torches -> coal, sticks
```

```haskell
λ> trace ["string"] recipes
fromList ["wool"]
λ> trace ["string", "torches"] recipes
fromList ["coal","sticks","wool"]
λ> extract $ recipes =>> trace ["torches"]
fromList ["coal","sticks"]
```

---

```haskell
string  -> wool
sticks  -> wood
bow     -> sticks, string
arrow   -> sticks, feather, stone
quiver  -> arrow, bow
torches -> coal, sticks
```

```haskell
λ> extract $ recipes =>> traces id
fromList []
λ> trace ["quiver"] $ recipes
fromList ["arrows","bow"]
λ> trace ["quiver"] $ recipes =>> traces id
fromList ["arrows","bow","feathers","sticks","stone","string"]
λ> trace ["quiver"] $ recipes =>> traces id =>> traces id
fromList ["arrows","bow","feathers","sticks","stone","string","wood","wool"]
```

---

![inline](./images/dep-analysis/quiver.png)

---

![inline](./images/dep-analysis/trace-quiver.png)

---

![inline](./images/dep-analysis/traces-quiver.png)

---

Notation
# **BONUS** (reader monad)

```haskell
(=>=) :: (w a -> b) -> (w b -> c) -> w a -> c

λ> ix 2 =>= takeS 3 $ countStream
[3,4,5]
```

---
