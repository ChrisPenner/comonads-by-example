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
projections :: Sum Int ->  Float
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

[.code-highlight: 1-5]
[.code-highlight: 1-8]
[.code-highlight: all]
```haskell
previousMonth = trace (Sum (-1))
nextMonth = trace (Sum 1)

λ> reportConfig & extract
100.0

λ> reportConfig =>> nextMonth  & extract
120.00001

λ> reportConfig =>> nextMonth =>> nextMonth =>> previousMonth & extract
120.00001
```

---

Make a simple report

[.code-highlight: 1]
[.code-highlight: 3]
[.code-highlight: 4-5]
[.code-highlight: 6-9]
[.code-highlight: all]
```haskell
detailedReport :: (ComonadTraced (Sum Int) w) => w Float -> String
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

```haskell
λ> buildHeader reportConfig 
"Please find enclosed your DETAILED report\n"
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
            => w Float -> String
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

[.code-highlight: 1-6]
[.code-highlight: 7-12]
[.code-highlight: 13-18]
[.code-highlight: all]
```haskell
λ> putStrLn $ buildReport reportConfig
Please find enclosed your DETAILED report:
This months sales in totality are: 100.0
Previous month's sales: 100.0
Next month's projections: 120.0

λ> putStrLn $ reportConfig =>> nextMonth =>> buildReport & extract
Please find enclosed your DETAILED report:
This months sales in totality are: 120.0
Previous month's sales: 100.0
Next month's projections: 144.0

λ> putStrLn $ reportConfig =>> nextMonth =>> buildReport . local (const Summary) & extract
Please find enclosed your SUMMARY report:
We achieved 120.0 in sales!
```

---

#[fit] **Questions**?

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

projections :: Region -> Sum Int ->  Float
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

reportConfig 
  :: EnvT  ReportStyle (TracedT (Sum Int) (Store Region)) Float
reportConfig = (EnvT Detailed (TracedT (store projections UK)))

```

---

#[fit]Now that we have a **store**
#[fit]We can **experiment** in other regions

---

```haskell
otherRegions :: (ComonadStore Region w) => w a -> [a]
otherRegions w = experiment others w
  where
    others currentRegion = filter (/= currentRegion) allRegions

allRegions :: [Region]
allRegions = [UK, America, Germany]
```

---

```haskell
projections UK      (Sum month) = 1.2 ^ (max 0 month) * 100
projections America (Sum month) = 1.3 ^ (max 0 month) * 200
projections Germany (Sum month) = 1.5 ^ (max 0 month) * 300

λ> extract reportConfig
100.0

-- others UK == [America, Germany]
λ> otherRegions reportConfig
[200.0,300.0]
```

---

#[fit] Using both **Store**
#[fit] and **Traced**
### We can build regional reports for other months!

---

[.code-highlight: 1-2]
[.code-highlight: 1-5]
[.code-highlight: 1-8]
[.code-highlight: all]
```haskell
λ> reportConfig & otherRegions
[200.0,300.0]

λ> reportConfig  =>> otherRegions =>> trace 3 & extract
[439.39996,1012.5]

λ> reportConfig  =>> trace 3 =>> otherRegions & extract
[439.39996,1012.5]

λ> reportConfig  =>> trace 1 =>> otherRegions =>> trace 2 & extract
[439.39996,1012.5]
```

---

#[fit] **Extending** **Trace**
#[fit] **Shifts** us to different **views**

---

#[fit] Every **view** is kept up to date
#[fit] **lazily**


---

```haskell
-- otherRegions :: (ComonadStore Region w) => w a -> [a]

comparisonReport :: (ComonadTraced (Sum Int) w, ComonadStore Region w) 
                 => w Float -> String
comparisonReport w =
    let otherReports = w =>> detailedReport =>> otherRegions & extract
     in "Comparison Report\n" <> unlines otherReports
```

---

```haskell
λ> putStrLn $ comparisonReport reportConfig
Comparison Report
America:
This months sales in totality are: 200.0
Previous month's sales: 200.0
Next month's projections: 260.0

Germany:
This months sales in totality are: 300.0
Previous month's sales: 300.0
Next month's projections: 450.0
```


---

[.code-highlight: 1,2,11]
[.code-highlight: all]
```haskell
buildReport :: (ComonadTraced (Sum Int) w, ComonadEnv ReportStyle w, ComonadStore Region w) 
            => w Float -> String
buildReport = do
    header <- buildHeader
    salesAmt <- extract
    style <- ask
    case style of
        Summary -> return $ header <> "We achieved " <> show salesAmt <> " in sales!"
        Detailed -> do
            rpt <- detailedReport
            compReport <- comparisonReport
            return $ header <> rpt <> "\n" <> compReport
```

---

```haskell
λ> putStrLn $ buildReport reportConfig
Please find enclosed your DETAILED report
UK:
This months sales in totality are: 100.0
Previous month's sales: 100.0
Next month's projections: 120.00001

Comparison Report
America:
This months sales in totality are: 200.0
Previous month's sales: 200.0
Next month's projections: 260.0

Germany:
This months sales in totality are: 300.0
Previous month's sales: 300.0
Next month's projections: 450.0
```

---

```haskell
λ> putStrLn $ reportConfig =>> trace 3 =>> buildReport & extract
Please find enclosed your DETAILED report
UK:
This months sales in totality are: 172.80002
Previous month's sales: 144.0
Next month's projections: 207.36

Comparison Report
America:
This months sales in totality are: 439.39996
Previous month's sales: 337.99997
Next month's projections: 571.21985

Germany:
This months sales in totality are: 1012.5
Previous month's sales: 675.0
Next month's projections: 1518.75
```

---

```haskell
λ> let monthlyReports = reportConfig =>> buildReport
λ> putStrLn $ peek Germany monthlyReports
Please find enclosed your DETAILED report
Germany:
This months sales in totality are: 300.0
Previous month's sales: 300.0
Next month's projections: 450.0

Comparison Report
UK:
This months sales in totality are: 100.0
Previous month's sales: 100.0
Next month's projections: 120.00001

America:
This months sales in totality are: 200.0
Previous month's sales: 200.0
Next month's projections: 260.0
```

---

[.code-highlight: 1]
[.code-highlight: 3]
[.code-highlight: 5]
[.code-highlight: 7]
[.code-highlight: all]

```haskell
λ> let monthlyReports = reportConfig =>> buildReport

λ> let germanyReport = peek Germany monthlyReports

λ> let germanyMarchReport = monthlyReports =>> peek Germany =>> trace 3 & extract

λ> let allFebruaryReports = monthlyReports =>> trace 2 =>> experiment (const allRegions)
```

---

##[fit] `chrispenner.ca`
##[fit] `github.com/ChrisPenner`
##[fit] `🐦 @ChrisLPenner`

