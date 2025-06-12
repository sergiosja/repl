# repl

I just really wanted to make a repl. Hopefully I'll find a nice name for it later:)

🗣️ Shoutout [Val](https://github.com/valbuild/val)

### What can it do?

It's loosely based on [R5RS: Legacy Scheme](https://docs.racket-lang.org/r5rs/index.html), the programming language with the prettiest syntax, objectively.

Here are some expressions:

```hs
repl, version 0.0.1

ь > 1
1

ь > '(1 2)
'(1 2)

ь > (+ 2 3 (* 1 (- 2 9) 9))
-58
```

Oh yeah, we use [polish notation](https://dl.acm.org/doi/pdf/10.5555/1074100.1074698) baby 😎

### Is that it?

Far from it. Here are some statements

```haskell
ь > (define currentYear 2025) -- I know ie the DrRacket repl doesn't return this. Sue me!
#<val:currentYear>

ь > (define lastYear 2024)
#<val:lastYear>

ь > (- currentYear lastYear)
1

ь > (define (plus x y) (+ x y))
#<procedure:plus>

ь > (define (plus1 x) (plus x 1))
#<procedure:plus1>

ь > (plus1 10)
11
```

### Wait, what's up with ь?

Oh, that's just a [мягкий знак](https://ru.wikipedia.org/wiki/%D0%AC) (soft sign), my favourite letter in the Cyrillic alphabet. It "serves as an indicator of palatalization of the preceding consonant" [[1]](#1).

### What else?

For debugging I find it valuable to print my program's AST. After the REPL became perfect I removed it, but then I got thinking, why not keep it on as a feature? So if you wanna see your program's AST you can prefix your programs with `ast`

```haskell
ь > ast 1
Expression (Constant (Number 1))

ь > ast x
Expression (Variable "x")

ь > ast (define x 1)
Statement (VariableDeclaration "x" (Number 1))
```

also I'm bad at keeping track of parentheses so in my perfect little world I'll allow as many as the eye can see

```haskell
ь > (cond ((#f 1) (#f 2) (#t 3)))))))
3
```

just like God intended

### Hmm, let me try this

Go to your terminal and run

```bash
stack install
```

and then I think

```bash
stack build
```

and if that goes well you can do

```bash
stack run
```

and you should be good. Or else your guess is as good as mine 😄

I'm just assuming you have stack and parsec and all that installed 😇😘

### Help, I'm stuck in the repl forever

Don't worry, you can actually leave whenever you want, you just have to say goodbye! Because an Irish goodbye would hurt its (my) feelings 😇

```haskell
repl, version 0.0.1

ь > exit()
Eval error: "Variable not found: exit"

ь > :quit
Parse error: (line 1, column 1):
unexpected ":"
expecting "(", identifier, literal string, float, integer, "#t", "#f" or "'"

ь > ciao
Arrivederci caro 👋
sergey@Sergeys-MBP repl %
```

This means you cannot have variables/procedures called `ciao`.. which is a well thought-out feature of course...

## References
<a id="1">[1]</a> 
Wikipedia, Wikimedia Foundation, 16 May 2025.
Soft sign.
https://en.wikipedia.org/wiki/Soft_sign