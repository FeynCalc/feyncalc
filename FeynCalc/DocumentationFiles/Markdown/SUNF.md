##  SUNF 

SUNF[a, b, c] are the structure constants of SU(N). The arguments a,b,c should be of symbolic type..

###  See also 

SUND, SUNDelta, SUNIndex, SUNSimplify, SUNT, Trick.

###  Examples 

```mathematica
SUNF[a, b, c] x + SUNF[b, a, c] y 
 
Calc[%] 
 
SUNSimplify[%%] 
 
SUNF[a, a, b] 
 
SUNF[a, a, b] // Calc
```

$$x f^{abc}+y f^{bac}$$

$$x f^{abc}-y f^{abc}$$

$$(x-y) f^{abc}$$

$$f^{aab}$$

$$0$$

This is a consequence of the usual choice for the normalization of the $T_a$ generators.

```mathematica
SUNF[a, b, c, Explicit -> True] 
 
SUNSimplify[SUNF[a, b, c] SUNF[a, b, d]] 
 
SUNSimplify[SUNF[a, b, c], Explicit -> True] 
 
SUNF[a, b, c] // StandardForm 
 
SUNF[a, b, c] // FCI // StandardForm 
 
SUNF[a, b, c] // FCI // FCE // StandardForm 
 
SUNF[b, a, c] 
 
SUNF[b, a, c] // FCI
```

$$2 i \left(\text{tr}(T^a.T^c.T^b)-\text{tr}(T^a.T^b.T^c)\right)$$

$$C_A \delta ^{cd}$$

$$-2 i \left(\text{tr}(T^a.T^b.T^c)-\text{tr}(T^b.T^a.T^c)\right)$$

```
(*SUNF[a, b, c]*)

(*SUNF[SUNIndex[a], SUNIndex[b], SUNIndex[c]]*)

(*SUNF[a, b, c]*)
```

$$f^{bac}$$

$$-f^{abc}$$