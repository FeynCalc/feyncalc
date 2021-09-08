## SISD

`SISD[p]` can be used as input for $D-1$-dimensional $\sigma^{\mu } p_{\mu }$ with $D$-dimensional Lorentz vector $p$ and is transformed into `PauliSigma[Momentum[p,D],D-1]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SIS](SIS.md).

### Examples

```mathematica
SISD[p]
```

$$\sigma \cdot p$$

```mathematica
SISD[p] // FCI // StandardForm

(*PauliSigma[Momentum[p, D], -1 + D]*)
```

```mathematica
SISD[p, q, r, s]
% // StandardForm
```

$$(\sigma \cdot p).(\sigma \cdot q).(\sigma \cdot r).(\sigma \cdot s)$$

```
(*SISD[p] . SISD[q] . SISD[r] . SISD[s]*)
```

```mathematica
SISD[q] . (SISD[p] + m) . SISD[q]
```

$$(\sigma \cdot q).(m+\sigma \cdot p).(\sigma \cdot q)$$
