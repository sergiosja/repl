# repl

I just really wanted to make a repl. Hopefully I'll find a nice name for it later:)

### What can it do?

It's loosely based on [R5RS: Legacy Scheme](https://docs.racket-lang.org/r5rs/index.html), the programming language with the objectively prettiest syntax.

You can execute values like numbers and quotes

```scheme
repl, version 0.0.1

ь > 1
1

ь > '(2 3 1 2 9 9)
'(2 3 1 2 9 9)
```

and expressions

```scheme
ь > (+ 2 3 1 2 9 9)
26
```

Oh yeah, we use [polish notation](https://dl.acm.org/doi/pdf/10.5555/1074100.1074698) baby 😎

### Is that it?

Far from it. We can define variables for later use

```scheme
ь > (define currentYear 2025)
#<var:currentYear=2025> -- I know ie the DrRacket repl doesn't return this but god forbid a boy has fun with it

ь > (define lastYear 2024)
#<var:lastYear=2024>

ь > (- currentYear lastYear)
1
```

Oh did you see that? Comments are supported too, of course 😎


### Wait, what's up with ь?

Oh, that's just a [мягкий знак](https://ru.wikipedia.org/wiki/%D0%AC) (soft sign), my favourite letter in the Cyrillic alphabet. It "serves as an indicator of palatalization of the preceding consonan" [[1]](#1).


### Hmm, let me try this

Go to your terminal and run

```
stack install
```

and then I think

```
stack build
```

and if that goes well you can do

```
stack run
```

and you should be good. Or else your guess is as good as mine 😄

I'm just assuming you have stack and parsec and all that installed 😇😘

### Help, I'm stuck in the repl forever

Don't worry, you can actually leave whenever you want, you just have to say goodbye! Because an Irish goodbye would hurt its (my) feelings 😇

```
repl, version 0.0.1

ь > "exit"
exit

ь > ":quit"
:quit

ь > ":wq"
:wq

ь > "leave"
leave

ь > ciao
sergey@Sergeys-MBP repl %
```

## References
<a id="1">[1]</a> 
Wikipedia, Wikimedia Foundation, 16 May 2025.
Soft sign.
https://en.wikipedia.org/wiki/Soft_sign