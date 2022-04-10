## CSISD

CSISD[p] can be used as input for D-1-dimensional $\sigma ^i p^i$ with D-1-dimensional Cartesian vector p and is transformed into PauliSigma[CartesianMomentum[p,D-1],D-1] by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSISD[p]
```

$$\sigma \cdot p$$

```mathematica
CSISD[p] // FCI // StandardForm

(*PauliSigma[CartesianMomentum[p, -1 + D], -1 + D]*)
```

```mathematica
CSISD[p, q, r, s]
```

$$(\sigma \cdot p).(\sigma \cdot q).(\sigma \cdot r).(\sigma \cdot s)$$

```mathematica
CSISD[p, q, r, s] // StandardForm

(*CSISD[p] . CSISD[q] . CSISD[r] . CSISD[s]*)
```