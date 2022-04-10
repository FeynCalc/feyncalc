## CSISE

CSISE[p] can be used as input for D-4-dimensional $\sigma^i p^i$ with $D-4$-dimensional Cartesian vector p and is transformed into `PauliSigma[CartesianMomentum[p,D-4],D-4]` by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSISE[p]
```

$$\hat{\sigma }\cdot \hat{p}$$

```mathematica
CSISE[p] // FCI // StandardForm

(*PauliSigma[CartesianMomentum[p, -4 + D], -4 + D]*)
```

```mathematica
CSISE[p, q, r, s]
```

$$\left(\hat{\sigma }\cdot \hat{p}\right).\left(\hat{\sigma }\cdot \hat{q}\right).\left(\hat{\sigma }\cdot \hat{r}\right).\left(\hat{\sigma }\cdot \hat{s}\right)$$

```mathematica
CSISE[p, q, r, s] // StandardForm

(*CSISE[p] . CSISE[q] . CSISE[r] . CSISE[s]*)
```