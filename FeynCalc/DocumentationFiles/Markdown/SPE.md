## SPE

`SPE[a, b]` denotes a $D-4$-dimensional scalar product. `SPE[a, b]` is transformed into `Pair[Momentum[a, -4 + D], Momentum[b, -4 + D]]` by `FeynCalcInternal`.

`SPE[p]` is the same as `SPE[p,p]`  $(=p^2)$.

### See also

[Overview](Extra/FeynCalc.md), [PD](PD.md), [Calc](Calc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [ScalarProduct](ScalarProduct.md), [SPD](SPD.md).

### Examples

```mathematica
SPE[p, q] + SPE[q]
```

$$\hat{p}\cdot \hat{q}+\hat{q}^2$$

```mathematica
SPE[p - q, q + 2 p]
```

$$(\hat{p}-\hat{q})\cdot (2 \hat{p}+\hat{q})$$

```mathematica
Calc[ SPE[p - q, q + 2 p] ]
```

$$-\hat{p}\cdot \hat{q}+2 \hat{p}^2-\hat{q}^2$$

```mathematica
ExpandScalarProduct[SPE[p - q]]
```

$$-2 \left(\hat{p}\cdot \hat{q}\right)+\hat{p}^2+\hat{q}^2$$

```mathematica
SPE[a, b] // StandardForm

(*SPE[a, b]*)
```

```mathematica
SPE[a, b] // FCI // StandardForm

(*Pair[Momentum[a, -4 + D], Momentum[b, -4 + D]]*)
```

```mathematica
SPE[a, b] // FCI // FCE // StandardForm

(*SPE[a, b]*)
```

```mathematica
FCE[ChangeDimension[SP[p, q], D - 4]] // StandardForm

(*SPE[p, q]*)
```
