build-lists: true
theme: Ostrich, 3

^ background-color: #0F0E0E
^ text: #FF5481
^ header: #FF5481
^ text-emphasis: #FFFFFF
^ text-strong: #FF5481
^ code: auto(25)

#[fit] **Comonads for (Kinda) Free: Cofree

---

#[fit] What's a __co__*free* comonad?

---

# *Free Monad* 

## a __Monad__ for any Functor

---

# *CoFree Comonad* 

## a __Comonad__ for any Functor

---

Let's look at some comonads we've already seen

---

```haskell
type Stream a = Cofree Identity a
```

---

```haskell
[0, 1, 2, 3, ...]
=~
0 :< Identity (1 :< Identity (2 :< Identity (3 :< ...)))
```

---

```haskell
count :: Cofree Identity Int
count = 0 :< Identity (fmap (+1) count)
```

---

```haskell
coiter :: Functor f 
       => (a -> f a) 
       -> a 
       -> Cofree f a
```

---

```haskell
count' :: Cofree Identity Int
count' = coiter next 0
  where
    next :: Int -> Identity Int
    next n = Identity (n + 1)
```

---

```haskell
unfold :: Functor f => (b -> (a, f b)) -> b -> Cofree f a
unfoldM :: (Traversable f, Monad m) => (b -> m (a, f b)) -> b -> m (Cofree f a)
```

---

```haskell
type ??? a = Cofree Maybe a
```

---

```haskell
type NonEmpty a = Cofree Maybe a
```

---

```haskell
type NonEmpty a = Cofree Maybe a

alphabet :: NonEmpty Char
alphabet = coiter maybeNext 'a'
  where
    maybeNext :: Char -> Maybe Char
    maybeNext 'z' = Nothing
    maybeNext a   = Just $ succ a
```

---

```haskell
type Tree a = Cofree ??? a
```
---

```haskell
type Tree a = Cofree [] a
```

---

```haskell
type Tree a = Cofree [] a

fileTree :: IO (Tree FilePath)
fileTree = unfoldM crawl "."
  where
    crawl :: FilePath -> IO (FilePath, [FilePath])
    crawl path = (,) path <$> listDirectory' path
    -- Just return an empty list if reading children fails,
    -- or if it's a file not a dir.
    listDirectory' :: FilePath -> IO [FilePath]
    listDirectory' path =
        listDirectory path <|> pure []
```

---

```haskell
λ> fileTree
"." :<
    [ "stack.yaml" :< []
    , "LICENSE" :< []
    , "CHANGELOG.md" :< []
    , "comonads-by-example.cabal" :< []
    , "README.md" :< []
    , "package.yaml" :< []
    , ".stack-work" :<
        [ "install" :< []
        , "dist" :< []
        , "logs" :< []
        , "ghci" :< []
        ]
    , "src" :<
        [ "UI" :< []
        , "Comonads" :< []
        , "UIPairing" :< []
        ]
    ]
```

---

```haskell
type ??? e a = Cofree (Const e) a
```

---

```haskell
type Env e a = Cofree (Const e) a
```

---

```haskell
type Store s a = Cofree ??? a
```

---

```haskell
type Store s a = Cofree (Compose ??? ???) a
```

---

```haskell
type Store s a = Cofree (Compose ??? ((->) s)) a
```

---

```haskell
type Store s a = Cofree (Compose ((,) s) ((->) s)) a
```

---

```haskell
type ??? m a = Cofree ((->) m) a
```

---

```haskell
type Traced m a = Cofree ((->) m) a -- When `m` is a Monoid
```

---

```haskell
hoistCofree :: Functor f => (forall x. f x -> g x) -> Cofree f a -> Cofree g a
```

---

# Zipper?

---

```haskell

```

---

# Comonad Transformers

---

```haskell
class ComonadTrans t where
lower :: Comonad w => t w a -> w a 
```

---

E.g.

```haskell
EnvT Int NonEmpty a

lower :: EnvT Int NonEmpty a -> NonEmpty a
```

---

# ZipperT

```haskell
ZipperT w a = ZipperT (Zipper (w a))

lower :: ZipperT w a -> w a
lower (ZipperT z) = extract z
```

---

# StoreT

```haskell
type Grid = StoreT (Store Int) Int a
```

---

```haskell
cohoist :: (Comonad w, Comonad v) => (forall x. w x -> v x) -> t w a -> t v a
```


---

Composing Objects using Day Convolution

https://blog.functorial.com/posts/2016-08-08-Comonad-And-Day-Convolution.html

--- 

Comonads as Spaces
https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html

---

Optionality

https://blog.functorial.com/posts/2017-10-28-Comonads-For-Optionality.html

---
