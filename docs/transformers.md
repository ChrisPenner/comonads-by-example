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

#[fit] **Comonad Transformers**
#### cobots in disguise

---

![inline](./images/wire.png)

`Hiring haskell devs/devops`

`wire.com/jobs`

---

#[fit] Comonad Transformers
#[fit] **Stack**
### Just like monad transformers

---

### An **environment** with a **store** inside

#[fit] `EnvT Settings (Store Int) Double`

---

#[fit] Just like Monad Transformers
#[fit] we can now access the **interface**
#[fit] for **ANY** comonad in the stack

---

A few examples

```haskell
ask   :: ComonadEnv    e w =>      w a -> e
trace :: ComonadTraced m w => m -> w a -> a
peek  :: ComonadStore  s w => s -> w a -> a
```

---

```haskell
class ComonadTrans t where
    lower :: Comonad w => t w a -> w a
```

---

#[fit] Monad Transformers **lift**
#[fit] Comonad Transformers **lower**

---

#[fit] You can **peel** off 
#[fit] **layers** 
#[fit] from the top down

---

EnvT

```haskell
data EnvT e w a = EnvT e (w a)

lower (EnvT e w) = w
```

---

StoreT

```haskell
data StoreT s w a = StoreT (w (s -> a)) s	

lower (StoreT wf s) = fmap ($ s) wf
```

---

TracedT

```haskell
newtype TracedT m w a = TracedT (w (m -> a))

lower (TracedT wf) = fmap ($ mempty) wf
```

---

#[fit] **Extract**
#[fit] on a transformer
#[fit] Goes **ALL** the way down

---

Example

```haskell
λ> let t = EnvT "hi" (traced (\(Sum x) -> x + 10))
t :: EnvT "hi" (Traced (Sum Int)) Int
λ> extract t
10

λ> trace (Sum 1) t
11

λ> ask t
"hi"
```

---

#[fit] Let's write 
#[fit] a report!

---

#[fit] We'll use an **Env**ironment
#[fit] to specify whether we want it to be
#[fit] **Detailed** or a **Summary**

---

```haskell
data ReportStyle = Detailed | Summary

reportConfig :: EnvT ReportStyle w a
reportConfig = EnvT Summary _
```

---

#[fit] We'll use a **Traced** Comonad
#[fit] to track which **month**
#[fit] the report is for

---

Setup

```haskell
-- Track sales projections over coming months
projections :: Sum Int ->  Double
projections (Sum month) = 1.2 ^ (max 0 month) * 100

reportConfig :: EnvT ReportStyle (Traced (Sum Int)) a
reportConfig = EnvT Detailed (traced projections)
```

---

Some queries

```haskell
previousMonth :: ComonadTraced (Sum Int) w => w a -> a
previousMonth = trace (Sum (-1))

nextMonth :: ComonadTraced (Sum Int) w => w a -> a
nextMonth = trace (Sum 1)
```

---

Make a simple report

[.code-highlight: 1]
[.code-highlight: 3]
[.code-highlight: 4-5]
[.code-highlight: 6-9]
[.code-highlight: all]
```haskell
detailedReport :: (ComonadTraced (Sum Int) w) => w Double -> String
detailedReport = do
    salesAmt <- extract
    prev <- previousMonth
    next <- nextMonth
    return $ unlines [ "This months sales in totality are: " <> show salesAmt
                     , "Previous month's sales: " <> show prev
                     , "Next month's projections: " <> show next
                     ]
```

---

[.code-highlight: 1]
[.code-highlight: 3]
[.code-highlight: all]
```haskell
buildHeader :: (ComonadEnv ReportStyle w) => w a-> String
buildHeader = do
    style <- ask
    return $ case style of
            Detailed -> "Please find enclosed your DETAILED report: \n"
            Summary -> "Please find enclosed your SUMMARY report: \n"
```

---


[.code-highlight: 1-2]
[.code-highlight: 4]
[.code-highlight: 5]
[.code-highlight: 6]
[.code-highlight: 7-12]
[.code-highlight: all]
```haskell
buildReport :: (ComonadTraced (Sum Int) w, ComonadEnv ReportStyle w) 
            => w Double -> String
buildReport = do
    header <- buildHeader
    salesAmt <- extract
    style <- ask
    case style of
        Summary -> 
          return $ header <> "We achieved " <> show salesAmt <> " in sales!"
        Detailed -> do
            rpt <- detailedReport
            return $ header <> rpt
```

---

#[fit] **Questions**?
#[fit] Want to try anything?

---

#[fit] Let's add 
#[fit] **one more layer**

---

#[fit] what if we want
#[fit] a report for
#[fit]  **each region**

---

[.code-highlight: 1-2]
[.code-highlight: all]
```haskell
data Region = America | UK | Germany
    deriving (Show, Eq, Ord)

projections :: Region -> Sum Int ->  Double
projections UK      (Sum month) = 1.2 ^ (max 0 month) * 100
projections America (Sum month) = 1.3 ^ (max 0 month) * 200
projections Germany (Sum month) = 1.5 ^ (max 0 month) * 300
```

---

Just play type tetris for this stuff...

```haskell
data    EnvT    e w a = EnvT e (w a)
newtype TracedT m w a = TracedT (w (m -> a))
data    StoreT  s w a = StoreT (w (s -> a)) s	

monthlyReport 
  :: EnvT  ReportStyle (TracedT (Sum Int) (Store Region)) Double
monthlyReport = (EnvT Detailed (TracedT (store projections UK)))

```

---


