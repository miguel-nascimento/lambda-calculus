# Untyped Lambda Calculus

> **TODO:** Combinators (S,K,I, Y, Omega)

> **TODO:** Encoding Power

> **TODO:** Beta, Alpha

It's the notion of a function in a computational perspective.

## How it works?

```
        ┼──────┼
 input  │      │ output
────────│  f   │─────────
        │      │
        ┼──────┼
```

I don't care what F is. F can be like a black box.
I only care about input and outputs.

## Syntax

Defining a +1 function: `λx.x+1`

`λx`: Defines a function with a variable x that will be used in the scope
`.`: defines a output
`x+1`: defines that with every x in the function, the output will be x + 1

Similarly, a basic sum function: `λx.λy.x+y`

If you want multiple arguments, remember, a function takes **one** value and return **one** output, so if you see a `λxy.x+y` this is just a syntax sugar to `λx.λy.x+y`

### Applying functions to literals

Lambda Calculus doesn't have any literal like integers, booleans, etc
But, to apply a function into another you can use something like this

```
(λx. x+1) (1)
```

Beta reduced to

```
λ1. 1+1
2
```

In the sum function:

```
((λx.λy. x+y) (1)) (2)
```

Beta reduced to

```
λ1.    λy.1+y  # this outputs a function
λ1.λ2.    1+2
3
```
