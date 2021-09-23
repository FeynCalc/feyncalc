## CGSD

`CGSD[p]` is transformed into `DiracGamma[CartesianMomentum[p, D-1], D]` by `FeynCalcInternal`.

`CGSD[p,q, ...]` is equivalent to `CGSD[p].CGSD[q]`.

### See also

[Overview](Extra/FeynCalc.md), [GSD](GSD.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
CGSD[p]
```

$$\gamma \cdot p$$

```mathematica
CGSD[p] // FCI // StandardForm

(*DiracGamma[CartesianMomentum[p, -1 + D], D]*)
```

```mathematica
CGSD[p, q, r, s]
```

$$(\gamma \cdot p).(\gamma \cdot q).(\gamma \cdot r).(\gamma \cdot s)$$

```mathematica
CGSD[p, q, r, s] // StandardForm

(*CGSD[p] . CGSD[q] . CGSD[r] . CGSD[s]*)
```

```mathematica
CGSD[q] . (CGSD[p] + m) . CGSD[q]
```

$$(\gamma \cdot q).(m+\gamma \cdot p).(\gamma \cdot q)$$
