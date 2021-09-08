## CSPE

`CSPE[p, q]` is the $D-4$-dimensional scalar product of `p` with `q` and is transformed into `CartesianPair[CartesianMomentum[p, D-4],CartesianMomentum[q, D-4]]` by `FeynCalcInternal`.

` CSPE[p]` is the same as `CSPE[p,p]` ( $=p^2$).

### See also

[Overview](Extra/FeynCalc.md), [SPE](SPE.md), [ScalarProduct](ScalarProduct.md), [CartesianScalarProduct](CartesianScalarProduct.md).

### Examples

```mathematica
CSPE[p, q] + CSPE[q]
```

$$\hat{p}\cdot \hat{q}+\hat{q}^2$$

```mathematica
CSPE[p - q, q + 2 p]
```

$$(\hat{p}-\hat{q})\cdot (2 \hat{p}+\hat{q})$$

```mathematica
Calc[ CSPE[p - q, q + 2 p] ]
```

$$-\hat{p}\cdot \hat{q}+2 \hat{p}^2-\hat{q}^2$$

```mathematica
ExpandScalarProduct[CSPE[p - q]]
```

$$-2 \left(\hat{p}\cdot \hat{q}\right)+\hat{p}^2+\hat{q}^2$$

```mathematica
CSPE[a, b] // StandardForm

(*CSPE[a, b]*)
```

```mathematica
CSPE[a, b] // FCI // StandardForm

(*CartesianPair[CartesianMomentum[a, -4 + D], CartesianMomentum[b, -4 + D]]*)
```

```mathematica
CSPE[a, b] // FCI // FCE // StandardForm

(*CSPE[a, b]*)
```

```mathematica
FCE[ChangeDimension[CSP[p, q], D - 4]] // StandardForm

(*CSPE[p, q]*)
```
