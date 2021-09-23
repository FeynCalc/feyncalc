## GSD

GSD[p] can be used as input for a $D$-dimensional $p^\mu \gamma_\mu$ and is transformed into `DiracGamma[Momentum[p,D],D]` by `FeynCalcInternal` (=`FCI`).

`GSD[p,q, ...]` is a short form for `GSD[p].GSD[q]`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [GAD](GAD.md).

### Examples

```mathematica
GSD[p]
```

$$\gamma \cdot p$$

```mathematica
GSD[p] // FCI // StandardForm

(*DiracGamma[Momentum[p, D], D]*)
```

```mathematica
GSD[p, q, r, s]
```

$$(\gamma \cdot p).(\gamma \cdot q).(\gamma \cdot r).(\gamma \cdot s)$$

```mathematica
GSD[p, q, r, s] // StandardForm

(*GSD[p] . GSD[q] . GSD[r] . GSD[s]*)
```

```mathematica
GSD[q] . (GSD[p] + m) . GSD[q]
```

$$(\gamma \cdot q).(m+\gamma \cdot p).(\gamma \cdot q)$$
