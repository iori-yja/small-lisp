Mini-Lisp in C

This is a very small and limited Lisp interpreter based on 
[Andru Luvisi's](http://www.sonoma.edu/users/l/luvisi/sl3.c). 
Its lightweightness
is very useful in learning how Lisp and Scheme internals work!

It's a single C source file which you can run like this:

```bash
gcc sl3.c && ((cat corelib.lisp; cat /dev/stdin) | ./a.out )
```

This will compile the interpreter, 
load the corelib and get you ready 
with a "repl" where you can experiment.
To quit this repl, type ^D (C-d) to enter EOF character.

Once the interpreter is running, you could try

```scheme
(+ 1 1)
(cdr '(1 2))
(is-prime 103)
(define X '(3 4 5))
(print 'first element 'of 'list 'X 'is (cdr X))
```

```Todo
- To port for MCU like OpenSPARC, Cortex-Mx or ARM7.
- Imprement copy GC.
```

```Portablility
- printf
- getc
- malloc
- exit
```
