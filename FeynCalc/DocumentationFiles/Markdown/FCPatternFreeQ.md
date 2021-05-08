##  FCPatternFreeQ 

FCPatternFreeQ[{exp}] yields True if {exp} does not contain any pattern objects, e.g. Pattern, Blank, BlankSequence and BlankNullSequence. FCPatternFreeQ[{exp},{h1,h2,...}] checks that in addition to the pattern objects, no heads h1, h2, ... are present..

###  Examples 

```mathematica
FCPatternFreeQ[{a}] 
 
FCPatternFreeQ[{a_}] 
 
FCPatternFreeQ[{g[x]}, {g}]
```

$$\text{True}$$

$$\text{False}$$

$$\text{False}$$