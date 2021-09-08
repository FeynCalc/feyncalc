## CSISE

CSISE[p] can be used as input for D-4-dimensional $\sigma^i p^i$ with $D-4$-dimensional Cartesian vector p and is transformed into `PauliSigma[CartesianMomentum[p,D-4],D-4]` by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSISE[p] 
 
CSISE[p] // FCI // StandardForm 
 
CSISE[p, q, r, s] 
 
CSISE[p, q, r, s] // StandardForm
```

$$\hat{\sigma }\cdot \hat{p}$$

```
(*PauliSigma[CartesianMomentum[p, -4 + D], -4 + D]*)
```

$$\left(\hat{\sigma }\cdot \hat{p}\right).\left(\hat{\sigma }\cdot \hat{q}\right).\left(\hat{\sigma }\cdot \hat{r}\right).\left(\hat{\sigma }\cdot \hat{s}\right)$$

```
(*CSISE[p] . CSISE[q] . CSISE[r] . CSISE[s]*)
```
