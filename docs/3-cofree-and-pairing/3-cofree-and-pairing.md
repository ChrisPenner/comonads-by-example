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
type ??? a = Cofree Maybe a
```

---

```haskell
type NonEmpty a = Cofree Maybe a
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
