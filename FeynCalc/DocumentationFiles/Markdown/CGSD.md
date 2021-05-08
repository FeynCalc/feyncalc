##  CGSD 

CGSD[p] is transformed into DiracGamma[CartesianMomentum[p, D-1], D] by FeynCalcInternal. CGSD[p,q, ...] is equivalent to CGSD[p].CGSD[q]. ....

###  See also 

GSD, DiracGamma.

###  Examples 

```mathematica
CGSD[p] 
 
CGSD[p] // FCI // StandardForm 
 
CGSD[p, q, r, s] 
 
CGSD[p, q, r, s] // StandardForm 
 
CGSD[q] . (CGSD[p] + m) . CGSD[q]
```

$$\gamma \cdot p$$

```
(*DiracGamma[CartesianMomentum[p, -1 + D], D]*)
```

$$(\gamma \cdot p).(\gamma \cdot q).(\gamma \cdot r).(\gamma \cdot s)$$

```
(*CGSD[p] . CGSD[q] . CGSD[r] . CGSD[s]*)
```

$$(\gamma \cdot q).(m+\gamma \cdot p).(\gamma \cdot q)$$