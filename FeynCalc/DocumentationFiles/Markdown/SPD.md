## SPD

`SPD[a, b]` denotes a $D$-dimensional scalar product.

 `SPD[a, b]` is transformed into `ScalarProduct[a, b,Dimension->D]` by `FeynCalcInternal`.

`SPD[p]` is the same as `SPD[p,p]` $(=p^2)$.

### See also

[Overview](Extra/FeynCalc.md), [PD](PD.md), [Calc](Calc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [ScalarProduct](ScalarProduct.md).

### Examples

```mathematica
SPD[p, q] + SPD[q]
```

$$p\cdot q+q^2$$

```mathematica
SPD[p - q, q + 2 p]
```

$$(p-q)\cdot (2 p+q)$$

```mathematica
Calc[ SPD[p - q, q + 2 p] ]
```

$$-p\cdot q+2 p^2-q^2$$

```mathematica
ExpandScalarProduct[SPD[p - q]]
```

$$-2 (p\cdot q)+p^2+q^2$$

```mathematica
SPD[a, b] // StandardForm

(*SPD[a, b]*)
```

```mathematica
SPD[a, b] // FCI // StandardForm

(*Pair[Momentum[a, D], Momentum[b, D]]*)
```

```mathematica
SPD[a, b] // FCI // FCE // StandardForm

(*SPD[a, b]*)
```

```mathematica
FCE[ChangeDimension[SP[p, q], D]] // StandardForm

(*SPD[p, q]*)
```
