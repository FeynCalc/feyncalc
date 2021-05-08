##  SUND 

SUND[a, b, c] are the symmetric SU(N) $d_{\text{abc}}.$.

###  See also 

SUNDelta, SUNF, SUNSimplify.

###  Examples 

```mathematica
SUND[a, b, c] 
 
SUND[a, b, c, Explicit -> True] 
 
SUND[c, a, b] 
 
SUND[a, b, b] 
 
SUNSimplify[SUND[a, b, c] SUND[a, b, c]] 
 
SUNSimplify[SUND[a, b, c] SUND[a, b, c], SUNNToCACF -> False] // Factor2 
 
SUNSimplify[SUND[a, b, c] SUND[e, b, c], SUNNToCACF -> False] // Factor2 
 
SUND[a, b, c] // StandardForm 
 
SUND[a, b, c] // FCI // StandardForm 
 
SUND[a, b, c] // FCI // FCE // StandardForm
```

$$d^{abc}$$

$$2 \left(\text{tr}(T^a.T^b.T^c)\right)+2 \left(\text{tr}(T^b.T^a.T^c)\right)$$

$$d^{abc}$$

$$d^{abb}$$

$$-2 \left(4-C_A^2\right) C_F$$

$$\frac{\left(1-N^2\right) \left(4-N^2\right)}{N}$$

$$-\frac{\left(4-N^2\right) \delta ^{ae}}{N}$$

```
(*SUND[a, b, c]*)

(*SUND[SUNIndex[a], SUNIndex[b], SUNIndex[c]]*)

(*SUND[a, b, c]*)
```