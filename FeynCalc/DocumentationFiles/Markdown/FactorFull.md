## FactorFull

`FactorFull` is an option of `Factor2` (default `False`). If set to `False`, products like `(a-b) (a+b)` will be replaced by `a^2-b^2`.

### See also

[Overview](Extra/FeynCalc.md), [Factor2](Factor2.md).

### Examples

```mathematica
Factor2[(a - b) (a + b) (c + d) (c - d)]
```

$$\left(a^2-b^2\right) \left(c^2-d^2\right)$$

```mathematica
Factor2[(a - b) (a + b) (c + d) (c - d), FactorFull -> True]
```

$$(a-b) (a+b) (c-d) (c+d)$$
