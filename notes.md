Talk about Comonads & Monads relation before talking about Duals

Give some high-level examples of comonadic operations to get peeps hooked

push details back

Drop extend ENTIRELY only talk about duplicate!

Comonad Laws: Talk about intuition

extend AS Query :)

Talk about elementwise vs context-wise

Introduce Ix in terms of Drop
THEN the other one

Less CODE more INTUITION and DIAGRAMS


> why do you mention duals? i dont know! is it because they are DUAL TO MONADS?
> zipper -> conceptually a doubly linked listed
> functions too? what _isn't_ a comonad?
> stream; discuss the data constructor; otherwise i promise you people will ask what is that magic syntax
> takeS slide: you said "lists are finite." but they're not cause hs is lazy
> input/windowedAvg/output. sorta confusing to have the inputs and outputs. maybe show "have" and "wanted"
> `(m a -> b) -> ma -> mb`... cant i just use pure? i'd suggest you say "we want to run this function AT EVERY POINT"
> comonad vs monad: ???/bind: what are the *s? maybe use a different color. put some spaces between the two so you can laser point more precisely
> the monad slide is TOO CONFUSING
> you were talking about bind but now you're talking about join
> monad vs comonad; using m for monads, but Stream for comonads
> duplicate slide; too thin lines for the head/tails. calling them `a'` and `b'` is confusing because i think we used to be talking about chars
> countStream comes out of nowhere. also this slide suggests you're doing it in ghci but no way!
> Real comonad slide. maybe hold off showing the monad, and then on a transition show beside so you can look at one, and then see the comparison
> explicitly mention that `w` is for comonads
> `instance` instead of `class`
> laws are probably clearer with cokleislis
> showing `Comonad` isntance for stream, probably you can just ignore `extend`. you told us w eonly need duplicate
> i like "now that we know them, lets look at a bunch of examples again"
> "nonempty lists"  say that these are _not streams_! because we spent a long time talking about something with the same pictures
> ix1 slide; show maybe `ix1 (a -> b -> ...) = b`
> "reshuffling the type sigs ix/dropS": awesome start. "oh shit i see!"
> "value-level" makes me think the alternative is "type-level". maybe call it "point-wise" or something?
> "should have duplicate instead of extend"
> cool! i saw the dropS in terms of ix, but not vv
> dropS1 slide is similarly confusing with its picture
> negStream has a bad name. maybe "hasSomeNegs"
