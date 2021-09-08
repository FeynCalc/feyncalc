## SISE

`SISE[p]` can be used as input for $D-4$-dimensional $\sigma ^{\mu } p_{\mu }$ with $D-4$-dimensional Lorentz vector $p$ and is transformed into `PauliSigma[Momentum[p,D-4], D-4]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [SIS](SIS.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
SISE[p]
```

$$\hat{\sigma }\cdot \hat{p}$$

```mathematica
SISE[p] // FCI // StandardForm

(*PauliSigma[Momentum[p, -4 + D], -4 + D]*)
```

```mathematica
SISE[p, q, r, s]
```

$$\left(\hat{\sigma }\cdot \hat{p}\right).\left(\hat{\sigma }\cdot \hat{q}\right).\left(\hat{\sigma }\cdot \hat{r}\right).\left(\hat{\sigma }\cdot \hat{s}\right)$$

```mathematica
SISE[p, q, r, s] // StandardForm

(*SISE[p] . SISE[q] . SISE[r] . SISE[s]*)
```
