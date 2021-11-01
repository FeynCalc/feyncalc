## ScalarProduct

`ScalarProduct[p, q]`  is the input for the scalar product of two Lorentz vectors p and q.

ScalarProduct[p] is equivalent to ScalarProduct[p, p].

Expansion of sums of momenta in `ScalarProduct` is done with `ExpandScalarProduct`.

Scalar products may be set, e.g. via `ScalarProduct[a, b] = m^2`; but `a` and `b` may not contain sums.

`ScalarProduct[a] ` corresponds to `ScalarProduct[a,a] `

Note that `ScalarProduct[a, b] = m^2` actually sets Lorentzian scalar products in different dimensions specified by the value of the `SetDimensions` option.

It is highly recommended to set `ScalarProduct`s before any calculation. This improves the performance of FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [FCClearScalarProducts](FCClearScalarProducts.md), [ExpandScalarProduct](ExpandScalarProduct.md), [ScalarProductCancel](ScalarProductCancel.md), [Pair](Pair.md), [SP](SP.md), [SPD](SPD.md).

### Examples

```mathematica
ScalarProduct[p, q]
```

$$\overline{p}\cdot \overline{q}$$

```mathematica
ScalarProduct[p + q, -q]
```

$$-\left(\overline{q}\cdot (\overline{p}+\overline{q})\right)$$

```mathematica
ScalarProduct[p, p]
```

$$\overline{p}^2$$

```mathematica
ScalarProduct[q]
```

$$\overline{q}^2$$

```mathematica
ScalarProduct[p, q] // StandardForm

(*Pair[Momentum[p], Momentum[q]]*)
```

```mathematica
ScalarProduct[p, q, Dimension -> D] // StandardForm

(*Pair[Momentum[p, D], Momentum[q, D]]*)
```

```mathematica
ScalarProduct[Subscript[p, 1], Subscript[p, 2]] = s/2
```

$$\frac{s}{2}$$

```mathematica
ExpandScalarProduct[ ScalarProduct[Subscript[p, 1] - q, Subscript[p, 2] - k]]
```

$$-\overline{k}\cdot \overline{p}_1+\overline{k}\cdot \overline{q}-\overline{q}\cdot \overline{p}_2+\frac{s}{2}$$

```mathematica
Calc[ ScalarProduct[Subscript[p, 1] - q, Subscript[p, 2] - k]]
```

$$-\overline{k}\cdot \overline{p}_1+\overline{k}\cdot \overline{q}-\overline{q}\cdot \overline{p}_2+\frac{s}{2}$$

```mathematica
ScalarProduct[q1] = qq;
```

```mathematica
SP[q1]
```

$$\text{qq}$$

```mathematica
FCClearScalarProducts[]
```