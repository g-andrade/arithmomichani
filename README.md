# arithmomichani

A command-line prefix calculator in Haskell, supporting the four elementary operations of arithmetic and sub-expressions.

There's absolutely no reason for you to ever consider using it - I only wrote it to learn some Haskell.

With that in mind, use [Stack](https://docs.haskellstack.org/en/stable/README/) to build it and run it.

## Capabilities

### Addition

```
% ./arithmomichani '+ 2 2'
Expression: Operation Sum [Number 2,Number 2]
Result: 4.0
```

```
$ ./arithmomichani '+ 2 2 3'
Expression: Operation Sum [Number 2,Number 2, Number 3]
Result: 7.0
```

### Subtraction

```
$ ./arithmomichani '- 2 5'
Expression: Operation Subtraction [Number 2,Number 5]
Result: -7.0
```

```
$ ./arithmomichani '- 2 5 42'
Expression: Operation Subtraction [Number 2,Number 5,Number 42]
Result: -49.0
```

### Multiplication

```
$ ./arithmomichani '* 3 3'
Expression: Operation Multiplication [Number 3,Number 3]
Result: 9.0
```

```
$ ./arithmomichani '* 3 3 3'
Expression: Operation Multiplication [Number 3,Number 3,Number 3]
Result: 27.0
```

### Division

```
$ ./arithmomichani '/ 10 5'
Expression: Operation Division [Number 10,Number 5]
Result: 2.0
```

```
$ ./arithmomichani '/ 10 5 2'
Expression: Operation Division [Number 10,Number 5,Number 2]
Result: 1.0
```

```
$ ./arithmomichani '/ 1 0'   
Expression: Operation Division [Number 1,Number 0]
Result: Infinity
```

### Subexpressions

A long-winded way of calculating 355 / 113 (which approximates π.)
```
$ ./arithmomichani '/ (* 710 2) 113 (+ 1 (- 3 2) 1 1)'                                                       
Expression: Operation Division [Operation Multiplication [Number 710, [...]]]
Result: 3.1415929203539825
```
