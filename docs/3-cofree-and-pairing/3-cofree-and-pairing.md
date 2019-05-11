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


# Comonad Transformers

---

```haskell
class ComonadTrans t where
lower :: Comonad w => t w a -> w a 
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
