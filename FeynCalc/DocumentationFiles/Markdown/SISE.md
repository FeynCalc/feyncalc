##  SISE 

SISE[p] can be used as input for D-4-dimensional $\sigma ^{\mu }p_{\mu }$ with D-4-dimensional Lorentz vector p and is transformed into PauliSigma[Momentum[p,D-4],D-4] by FeynCalcInternal..

###  Examples 

```mathematica
SISE[p] 
 
SISE[p] // FCI // StandardForm 
 
SISE[p, q, r, s] 
 
SISE[p, q, r, s] // StandardForm
```

$$\hat{\sigma }\cdot \hat{p}$$

```
(*PauliSigma[Momentum[p, -4 + D], -4 + D]*)
```

$$\left(\hat{\sigma }\cdot \hat{p}\right).\left(\hat{\sigma }\cdot \hat{q}\right).\left(\hat{\sigma }\cdot \hat{r}\right).\left(\hat{\sigma }\cdot \hat{s}\right)$$

```
(*SISE[p] . SISE[q] . SISE[r] . SISE[s]*)
```