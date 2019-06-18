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

#[fit] **Env Comonad**

---

![inline](./images/wire.png)

`Hiring haskell devs/devops`

`wire.com/jobs`

---

# Env

#[fit] Has a single **slot**
#[fit] Can **view** provided context
#[fit] Reader monad but **object** **oriented**

![right](./images/env/trees.jpg)

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

#[fit] We can **Query** our environment
#[fit] Using **ask**

---

# Ask

```haskell
ask :: Env e a -> e
ask (Env e _) = e

λ> ask (Env 42 "hello")
42
```

---

#[fit] We can select a subset of our environment
#[fit] Using ask**s**

---

```haskell
asks :: (e -> e') -> Env e a -> e'
asks f (Env e _) = f e

λ> asks fst (Env ("first", "second") 1337)
"first"

λ> asks snd (Env ("first", "second") 1337)
"second"
```

---

# example

```haskell
data Settings =
    Settings
    { padAmount :: Int
    , maxLength :: Int
    , padChar   :: Char
    } deriving (Show)
```

---

# Queries using **asks**

```haskell
data Settings =
    Settings
    { padAmount :: Int
    , maxLength :: Int
    , padChar   :: Char
    } deriving (Show)

getPadChar :: Env Settings a -> Int
getPadChar w = asks padChar w
```

---

# Using 'ask' queries

```haskell
context :: Env Settings String
context = 
  env (Settings{padAmount=3, maxLength=5, padChar='*'}) 
       "Hello World"

λ> getPadChar context
'*'
```

---

# Using 'asks' directly

```haskell
context :: Env Settings String
context = 
  env (Settings{padAmount=3, maxLength=5, padChar='*'}) 
       "Hello World"

λ> asks padAmount context
3
```

---

#[fit] Questions?

---

#[fit] Let's write some more
#[fit] **complex**
#[fit] **queries**

---


```haskell
trunc :: Env Settings [a] -> [a]
trunc w =
    let mxLngth = asks maxLength w
    in take mxLngth (extract w)
```

---

```haskell
context :: Env Settings String
context = 
  env (Settings{padAmount=3, maxLength=5, padChar='*'}) 
       "Hello World"

λ> trunc context
"Hello"
```

---


```haskell
pad :: Env Settings String -> String
pad w =
    let padAmt = asks padAmount w
        c      = asks padChar w
     in    replicate padAmt c 
        <> extract w 
        <> replicate padAmt c
```

---

```haskell
context = 
  env (Settings{padAmount=3, maxLength=5, padChar='*'}) 
       "Hello World"

λ> pad context
"***Hello World***"
```

---

#[fit] Chaining Env
#[fit] **Queries**
#[fit] into **pipelines**

---

Mutations aren't so interesting for Env
But we want to carry our context
through a series of computations

Just like **Reader**

---

# Remember `(=>=)`

```haskell
(=>=) :: (w a -> b) 
      -> (w b -> c) 
      -> (w a -> c)
```

---

#[fit]`=>=`
#[fit] Chains many **queries**
#[fit] into **ONE**

---


```haskell
pipeline :: Env Settings String -> String
pipeline = trunc =>= pad

λ> pipeline context
"***Hello***"
```

---

# Order matters

```haskell
pipeline2 :: Env Settings String -> String
pipeline2 = pad =>= trunc

λ> pipeline2 context
"***He"

```

---

#[fit] Local
## lets us **alter**
#[fit] The **environment**
## for a **query**

---

#[fit] `local :: (e -> e') -> Env e a -> Env e' a`

---

# We can temporarily change settings WITHIN the pipeline

```haskell
pipeline3 :: Env Settings String -> String
pipeline3 = trunc =>= pad . local (setPadChar '_') =>= pad

λ> pipeline3 context
"***___Hello___***"
```

---

#[fit] Questions?

---

Bonus

#[fit] Abusing 
#[fit] **Do-Notation**

---

do-notation provides the context on every line

[.code-highlight: 2-4,9-12]
[.code-highlight: 5,13]
[.code-highlight: 6,14]
[.code-highlight: 7,15]
[.code-highlight: all]
```haskell
pad :: Env Settings String -> String
pad w =
    let padAmt = asks padAmount w
        c      = asks padChar w
        txt    = extract w
        padding = replicate padAmt c
     in padding <> txt <> padding

pad' :: Env Settings String -> String
pad' = do
  padAmt <- asks padAmount
  c      <- asks padChar
  txt    <- extract
  let padding = replicate padAmt c 
  return $ padding <> txt <> padding
```

---

# Abusing even further

```haskell
pad'' :: Env Settings String -> String
pad'' = do
  padding <- replicate <$> asks padAmount <*> asks padChar
  txt     <- extract
  return $ padding <> txt <> padding
```

---

# ⛳️ Code Golf 🏌️‍♂️

```haskell
pad''' :: Env Settings String -> String
pad''' = do
  let padding = replicate <$> asks padAmount <*> asks padChar
  padding <> extract <> padding
```

---

##[fit] `chrispenner.ca`
##[fit] `github.com/ChrisPenner`
##[fit] `🐦 @ChrisLPenner`

