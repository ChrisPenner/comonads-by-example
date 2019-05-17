build-lists: true
theme: Ostrich, 3
slide-transition: true

^ background-color: #0F0E0E
^ text: #FF5481
^ header: #FF5481
^ text-emphasis: #FFFFFF
^ text-strong: #FF5481
^ code: auto(25)

#[fit]**_BYOZ_**

Build Your Own Zipper

---

```haskell
data Zipper a =
  Zipper
    { left :: [a]
    , focus :: a
    , right :: [a]
    } deriving (Show, Eq, Functor, Foldable)
```

---

[.code-highlight: 1-2]
[.code-highlight: all]
```haskell
λ> let z = fromList ['a', 'b', 'c', 'd', 'e']
Zipper []  'a' ['b', 'c', 'd', 'e']

λ> moveRight' . moveRight' $  z
Zipper ['b', 'a'] 'c' ['d', 'e']
```

![inline](./images/zipper.png)

---

```haskell
Zipper ['b', 'a'] 'c' ['d', 'e']
```

![inline](./images/zipper.png)
![inline](./images/zipper-l1.png)

```haskell
Zipper ['a'] 'b' ['c', 'd', 'e']
```


---

![inline](./images/zipper.png)
![inline](./images/zipper-l1.png)
![inline](./images/zipper-l2.png)

```haskell
Zipper [] 'a' ['b', 'c', 'd', 'e']
```
---

```haskell
Zipper { left = ['a'] , focus = 'b' , right = ['c'] }
```

![inline](./images/zipper-small.png)

---

![right fit](./images/zipper-duplicate.png)
![left fit](./images/zipper-small.png)

---


[.code-highlight: 1-2]
[.code-highlight: 1-2, 4]
[.code-highlight: 1-2, 6-12]
[.code-highlight: 1-2, 13-17]
[.code-highlight: 1-2, 18-24]
[.code-highlight: all]
```haskell
λ> z
Zipper {left = "a", focus = 'b', right = "c"}

λ> duplicate z
Zipper
    { left =
        [ Zipper
            { left = []
            , focus = 'a'
            , right = ['b', 'c' ]
            }
        ]
    , focus = Zipper
        { left = ['a']
        , focus = 'b'
        , right = ['c']
        }
    , right =
        [ Zipper
            { left = ['b', 'a']
            , focus = 'c'
            , right = [] 
            }
        ]
    }
```

---

# HOMEWORK
## Implement Comonad For Zipper

```haskell
instance Comonad Zipper where
  extract :: Zipper a -> a
  duplicate :: Zipper a -> Zipper (Zipper a)
  extend :: (Zipper a -> b) -> Zipper a -> Zipper b
```

---

# Maybe break? Review?

---

# Rainwater Problem

---

![fit](./images/rainwater.png)

---

#[fit] **LIVE CODE IT**

### *what could possibly go wrong?*

---

TODO: Add diagram showing zipper extend solution

---


```haskell
problem :: Zipper Int
problem = fromList [2, 0, 4, 2, 3, 2, 1, 2]

waterAtPosition :: Zipper Int -> Int
waterAtPosition (Zipper toLeft current toRight) = max 0 (min maxLeft maxRight - current)
  where
    maxLeft  = maximum (0 : toLeft)
    maxRight = maximum (0 : toRight)

solution :: Zipper Int -> Int
solution = sum . extend waterAtPosition
```

---

![fit](./images/questions/any-questions-dwight.gif)

---

THE END 

---


---

# Grids


![inline](./images/grids/grid-selected.png)

---

![inline](./images/grids/duplicate-grid.png)

---

![inline](./images/grids/duplicate-grid-selected.png)

---

![inline](./images/grids/sudoku-rules.png)

---

![inline](./images/tree-demo-1.png)

---

![original fit](./images/tree-demo-1.png)
![original fit](./images/tree-demo-2.png)

---

# ...

---

```haskell
  value :: Tree a -> a

  duplicateTree :: Tree a -> Tree (Tree a)

  mapSubtrees :: (Tree a -> b) -> Tree a -> Tree b
```

---

```haskell
  extract :: Tree a -> a

  duplicate :: Tree a -> Tree (Tree a)

  extend :: (Tree a -> b) -> Tree a -> Tree b
```

---

```haskell
class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)

  extend :: (w a -> b) -> w a -> w b
```

---

```haskell
class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)

  extend :: (w a -> b) -> w a -> w b
```

```haskell
class Applicative m => Monad m where
  return :: a -> m a

  join :: m (m a) -> m a

  bind :: m a -> (a -> m b) -> m b
```

---

```haskell
  extract :: w a ->   a
    vs
  return  ::   a -> m a

```

---
```haskell
  duplicate ::    w a  -> w (w a)
   vs
  join      :: m (m a) ->    m a
```

---

```haskell
  extend :: (w a -> b) -> w a -> w b
   vs
  bind   :: m a -> (a -> m b) -> m b
```
---

```
    #    
    # o #  
# o # # # # o #
# o # # # # # #
- - - - - - - -
2 0 4 2 3 2 1 2
```
