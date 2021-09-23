## CSISD

CSISD[p] can be used as input for D-1-dimensional $\sigma ^ip^i$ with D-1-dimensional Cartesian vector p and is transformed into PauliSigma[CartesianMomentum[p,D-1],D-1] by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSISD[p] 
 
CSISD[p] // FCI // StandardForm 
 
CSISD[p, q, r, s] 
 
CSISD[p, q, r, s] // StandardForm
```

$$\sigma \cdot p$$

```
(*PauliSigma[CartesianMomentum[p, -1 + D], -1 + D]*)
```

$$(\sigma \cdot p).(\sigma \cdot q).(\sigma \cdot r).(\sigma \cdot s)$$

```
(*CSISD[p] . CSISD[q] . CSISD[r] . CSISD[s]*)
```
