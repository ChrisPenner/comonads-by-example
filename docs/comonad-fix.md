build-lists: true
theme: Ostrich, 3
slide-transition: true
slidenumbers: true

^ background-color: #0F0E0E
^ text: #FF5481
^ header: #FF5481
^ text-emphasis: #FFFFFF
^ text-strong: #FF5481
^ code: auto(25)

# Comonad Fix!

```haskell
wfix :: Comonad w => w (w a -> a) -> a
```

---

# Factorial

```haskell
> factorial 3 
3 * 2 * 1
6
```

---

# Factorial

```haskell
let fact 0 = 1
    fact n = n * fact (n -1)

> fact 3
6
```

---

# Factorial

```haskell
factorialStore :: Store Int Int
factorialStore = extend wfix (store go 0)
    where
      go :: Int -> (Store Int Int -> Int)
      go 0 _ = 1
      go n w = n * peek (n - 1) w
```

---



-- | Sum of current and following numbers at each position
-- λ> sums (NE.fromList [1,2,3,4])
-- 10 :| [9,7,4]
sums :: NE.NonEmpty Int -> NE.NonEmpty Int
sums w = extend wfix (go <$> w)
  where
    go :: Int -> NE.NonEmpty Int -> Int
    go n (_ NE.:| []) = n
    go n (_ NE.:| (x:_)) = n + x

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

```haskell
recipes :: Traced (S.Set String) (S.Set String)
recipes = traced (foldMap ingredientsOf)

allIngredientsFor :: Traced (S.Set String) (S.Set String)
allIngredientsFor = extend wfix (selectNext <$> listen recipes)
  where
    selectNext :: (S.Set String, S.Set String)
               -> Traced (S.Set String) (S.Set String)
               -> S.Set String
    selectNext (requirements, input) t
        | S.null (S.difference requirements input) = input
        | otherwise = trace (S.difference requirements input) t
```

---

# Spreadsheets

![](./images/spreadsheets/example-spreadsheet.png)

---

# Spreadsheets

![fit inline](./images/spreadsheets/example-spreadsheet.png)

---

# Formulas

![fit inline](./images/spreadsheets/item-cost.png)

---

# Formulas

![fit inline](./images/spreadsheets/spreadsheet-formula.png)

---

# Formulas

![fit inline](./images/spreadsheets/total-cost.png)

---

```haskell
dataDef :: (Char, Int) -> Double
dataDef ('B', 2) = 1
dataDef ('B', 3) = 0.75
dataDef ('B', 4) = 2

dataDef ('C', 2) = 7
dataDef ('C', 3) = 5
dataDef ('C', 4) = 9
dataDef _ = 0
```

![fit right](./images/spreadsheets/example-spreadsheet.png)

---

```haskell
dataDef :: (Char, Int) -> Double
dataDef ('A', 0) = 1
dataDef ('A', 1) = 0.75
dataDef ('A', 2) = 2

dataDef ('B', 0) = 7
dataDef ('B', 1) = 5
dataDef ('B', 2) = 9
dataDef _ = 0
```

![fit right](./images/spreadsheets/item-cost.png)
