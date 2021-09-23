## GS

`GS[p]` can be used as input for a 4-dimensional $p^\mu \gamma_\mu$ and is transformed into `DiracGamma[Momentum[p]]` by `FeynCalcInternal` (=`FCI`).

`GS[p,q, ...]` is a short form for `GS[p].GS[q]`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [GAD](GAD.md).

### Examples

```mathematica
GS[p]
```

$$\bar{\gamma }\cdot \overline{p}$$

```mathematica
GS[p] // FCI // StandardForm

(*DiracGamma[Momentum[p]]*)
```

```mathematica
GS[p, q, r, s]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{r}\right).\left(\bar{\gamma }\cdot \overline{s}\right)$$

```mathematica
GS[p, q, r, s] // StandardForm

(*GS[p] . GS[q] . GS[r] . GS[s]*)
```

```mathematica
GS[q] . (GS[p] + m) . GS[q]
```

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}+m\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$
