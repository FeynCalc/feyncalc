## SP

`SP[a, b]` denotes a $4$-dimensional scalar product. `SP[a, b]` is transformed into `ScalarProduct[a, b]` by `FeynCalcInternal`.

`SP[p]` is the same as `SP[p, p]` $(=p^2)$.

### See also

[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [ScalarProduct](ScalarProduct.md).

### Examples

```mathematica
SP[p, q] + SP[q]
```

$$\overline{p}\cdot \overline{q}+\overline{q}^2$$

```mathematica
SP[p - q, q + 2 p]
```

$$(\overline{p}-\overline{q})\cdot (2 \overline{p}+\overline{q})$$

```mathematica
Calc[ SP[p - q, q + 2 p] ]
```

$$-\overline{p}\cdot \overline{q}+2 \overline{p}^2-\overline{q}^2$$

```mathematica
ExpandScalarProduct[SP[p - q]]
```

$$-2 \left(\overline{p}\cdot \overline{q}\right)+\overline{p}^2+\overline{q}^2$$

```mathematica
SP[a, b] // StandardForm

(*SP[a, b]*)
```

```mathematica
SP[a, b] // FCI // StandardForm

(*Pair[Momentum[a], Momentum[b]]*)
```

```mathematica
SP[a, b] // FCI // FCE // StandardForm

(*SP[a, b]*)
```
