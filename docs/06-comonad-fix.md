build-lists: true
theme: Ostrich, 3
slide-transition: true
slidenumbers: true
footer: `💻 github.com/ChrisPenner/comonads-by-example | 🐦 @ChrisLPenner | 📝 chrispenner.ca`

^ background-color: #0F0E0E
^ text: #FF5481
^ header: #FF5481
^ text-emphasis: #FFFFFF
^ text-strong: #FF5481
^ code: auto(25)

#[fit] Comonad **Fix**!
### _The truth was inside you all along_

---

#[fit]Given the **answer**
#[fit]compute the **answer**

####seriously Haskell WTF?

---

#[fit]`wfix :: w (w a -> a) -> a `

---

##[fit] Given a **comonad**
##[fit] Filled with 
##[fit]**queries over the final result**

---

##[fit] resolve them all **using** the result
##[fit] to **get** the result

---

![fit](./images/fix/mind-blown.gif)

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
-- The FUNCTION is the context 
-- which we can use in the computation
let fact 0 = 1
    fact n = n * fact (n -1)

> fact 3
6
```

---

#[fit] Note how `fact 0 = 1`
#[fit] **DOESN'T** reference the **context**
###_a.k.a. it doesn't recurse_

---

# Factorial

```haskell
factorialTraced :: Traced (Sum Int) Int
factorialTraced = extend wfix (traced go)
  where
    go :: Sum Int -> Traced (Sum Int) Int -> Int
    go (Sum 0) _ = 1
    go (Sum n) t = n * trace (-1) t
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

dataDef2 ('D',  row) = ???

dataDef _ = 0
```

![fit right](./images/spreadsheets/item-cost.png)

---

```haskell
dataDef2 ('D',  row) w | row < 6 =
  let price = peek ('B', row) w
      quant = peek ('C', row) w
   in price * quant
```

![fit right](./images/spreadsheets/item-cost.png)

---

```haskell
getCells :: Functor f => f s -> Store s a -> f a
getCells cells w = experiment (const cells) w

-- Tax
dataDef2 ('D', 6) _ = 0.15
-- Total
dataDef2 ('D', 7) w = sum . getCells (('D',) <$> [1..5]) $ w
-- Total With Tax
dataDef2 ('D', 8) w =
    let tax = peek ('D', 6) w
        total = peek ('D', 7) w
     in (tax * total) + total
```

