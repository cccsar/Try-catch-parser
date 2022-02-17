#Try-catch-parser

Recursive L-attributed descent parser implementation for typed try-catch expressions of the form:

```
I -> try I catch I finally I
   | try I catch I
   | I ; I
   | instr_$upper 
```

Where `instr_$upper` is the terminal representing an instruction with it's type being `$upper`.

The parser returns the expected type for a valid try-catch expression. That is, the type of the last expression to be (possibly) executed.

That can be, either an actually single __last__ instruction type or the type related to a `try-catch` block.

---

This is based on the L-attribued grammar

```
   S -> B       { S.type <- B.type }
   B -> I C     { C.pre  <- I.type 
                  B.type <- C.type }
   C -> ; I C1  { C1.pre <- I.type      
                  C.type <- C1.type }
      | lambda  { C.type <- C.pre }
   I -> tB1cB2J { J.pre  <- Either B1.type B2.type 
                  I.type <- J.type }
      | i       { I.type <- i.type }  
   J -> fB      { J.type <- B.type }
      | lambda  { J.type <- J.pre }
```

used to compute types, and also, the corresponding lookaheads:

```
                  Lookahead
                ------------
   S -> B       | t,i
   B -> I C     | t,i
   C -> ; I C   | ;
      | lambda  | c,f,$ 
   I -> tBcBJ   | t
      | i       | i
   J -> fB      | f 
      | lambda  | c,;,$
```

used to properly parse and provide specific error messages.

---

To test funtionality simply run 

```
runhaskell TryCatch.hs
```

and then input a try-catch expression to get it's type.

> Sample expressions with all error cases are under `samples`, and can be tested by running the script `./runSamples`
