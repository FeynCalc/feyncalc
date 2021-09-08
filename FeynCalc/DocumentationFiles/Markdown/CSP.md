## CSP

`CSP[p, q]` is the 3-dimensional scalar product of `p` with `q` and is transformed into `CartesianPair[CartesianMomentum[p],CartesianMomentum[q]]` by `FeynCalcInternal`.

` CSP[p]` is the same as `CSP[p,p]` ($=p^2$).

### See also

[Overview](Extra/FeynCalc.md), [SP](SP.md), [ScalarProduct](ScalarProduct.md), [CartesianScalarProduct](CartesianScalarProduct.md).

### Examples

```mathematica
CSP[p, q] + CSP[q]
```

$$\overline{p}\cdot \overline{q}+\overline{q}^2$$

```mathematica
CSP[p - q, q + 2 p]
```

$$(\overline{p}-\overline{q})\cdot (2 \overline{p}+\overline{q})$$

```mathematica
Calc[ CSP[p - q, q + 2 p] ]
```

$$-\overline{p}\cdot \overline{q}+2 \overline{p}^2-\overline{q}^2$$

```mathematica
ExpandScalarProduct[CSP[p - q]]
```

$$-2 \left(\overline{p}\cdot \overline{q}\right)+\overline{p}^2+\overline{q}^2$$

```mathematica
CSP[a, b] // StandardForm

(*CSP[a, b]*)
```

```mathematica
CSP[a, b] // FCI // StandardForm

(*CartesianPair[CartesianMomentum[a], CartesianMomentum[b]]*)
```

```mathematica
CSP[a, b] // FCI // FCE // StandardForm

(*CSP[a, b]*)
```
