##  SISD 

SISD[p] can be used as input for D-1-dimensional $\sigma ^{\mu }p_{\mu }$ with D-dimensional Lorentz vector p and is transformed into PauliSigma[Momentum[p,D],D-1] by FeynCalcInternal..

###  Examples 

```mathematica
SISD[p] 
 
SISD[p] // FCI // StandardForm 
 
SISD[p, q, r, s] 
 
SISD[p, q, r, s] // StandardForm 
 
SISD[q] . (SISD[p] + m) . SISD[q]
```

$$\sigma \cdot p$$

```
(*PauliSigma[Momentum[p, D], -1 + D]*)
```

$$(\sigma \cdot p).(\sigma \cdot q).(\sigma \cdot r).(\sigma \cdot s)$$

```
(*SISD[p] . SISD[q] . SISD[r] . SISD[s]*)
```

$$(\sigma \cdot q).(m+\sigma \cdot p).(\sigma \cdot q)$$