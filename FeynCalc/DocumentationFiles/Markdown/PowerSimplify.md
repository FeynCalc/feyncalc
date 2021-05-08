##  PowerSimplify 

PowerSimplify[exp] simplifies$(-x)^a$ to $(-1)^a$ $x^a$ and $(y-x)^n$ to $(-1)^n$ $(x-y)^n$; thus assuming that the exponent is an integer (even if it is symbolic). Furthermore $(-1)^{a+n}$ and $i^{a+n}$ are expanded and $i^{2 m}$ -> $(-1)^m$ and (-1)^(n m) -> 1 and (-1)^(n m) -> $(-1)^m$ for n even and odd respectively and $(-1)^{-n}$ -> $(-1)^n$ and $e^{im\pi }$ -> $(-1)^m$..

###  See also 

DataType, OPEm.

###  Examples 

```mathematica
PowerSimplify[(-1)^(2 OPEm)] 
 
PowerSimplify[(-1)^(OPEm + 2)] 
 
PowerSimplify[(-1)^(OPEm - 2)] 
 
PowerSimplify[I^(2 OPEm)]
```

$$1$$

$$(-1)^m$$

$$(-1)^m$$

$$(-1)^m$$