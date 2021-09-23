## CGSE

`CGSE[p]` is transformed into `DiracGamma[CartesianMomentum[p, D-4], D-4]` by FeynCalcInternal.

`CGSE[p,q, ...]` is equivalent to `CGSE[p].CGSE[q]`.

### See also

[Overview](Extra/FeynCalc.md), [GSE](GSE.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
CGSE[p]
```

$$\hat{\gamma }\cdot \hat{p}$$

```mathematica
CGSE[p] // FCI // StandardForm

(*DiracGamma[CartesianMomentum[p, -4 + D], -4 + D]*)
```

```mathematica
CGSE[p, q, r, s]
```

$$\left(\hat{\gamma }\cdot \hat{p}\right).\left(\hat{\gamma }\cdot \hat{q}\right).\left(\hat{\gamma }\cdot \hat{r}\right).\left(\hat{\gamma }\cdot \hat{s}\right)$$

```mathematica
CGSE[p, q, r, s] // StandardForm

(*CGSE[p] . CGSE[q] . CGSE[r] . CGSE[s]*)
```

```mathematica
CGSE[q] . (CGSE[p] + m) . CGSE[q]
```

$$\left(\hat{\gamma }\cdot \hat{q}\right).\left(m+\hat{\gamma }\cdot \hat{p}\right).\left(\hat{\gamma }\cdot \hat{q}\right)$$
