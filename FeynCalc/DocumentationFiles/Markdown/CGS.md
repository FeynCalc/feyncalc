## CGS

`CGS[p]` is transformed into `DiracGamma[CartesianMomentum[p]]` by `FeynCalcInternal`.

`CGS[p,q, ...]` is equivalent to `CGS[p].CGS[q]`.

### See also

[Overview](Extra/FeynCalc.md), [GS](GS.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
CGS[p]
```

$$\overline{\gamma }\cdot \overline{p}$$

```mathematica
CGS[p] // FCI // StandardForm

(*DiracGamma[CartesianMomentum[p]]*)
```

```mathematica
CGS[p, q, r, s]
```

$$\left(\overline{\gamma }\cdot \overline{p}\right).\left(\overline{\gamma }\cdot \overline{q}\right).\left(\overline{\gamma }\cdot \overline{r}\right).\left(\overline{\gamma }\cdot \overline{s}\right)$$

```mathematica
CGS[p, q, r, s] // StandardForm

(*CGS[p] . CGS[q] . CGS[r] . CGS[s]*)
```

```mathematica
CGS[q] . (CGS[p] + m) . CGS[q]
```

$$\left(\overline{\gamma }\cdot \overline{q}\right).\left(\overline{\gamma }\cdot \overline{p}+m\right).\left(\overline{\gamma }\cdot \overline{q}\right)$$
