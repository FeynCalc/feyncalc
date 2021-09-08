## Solve3

`Solve3` is equivalent to `Solve`, except that it works only for linear equations (and returns just a list) and uses the "high school algorithm.

Sometimes it is better than `Solve` for systems involving rational polynomials.

### See also

[Overview](Extra/FeynCalc.md), [Solve2](Solve2.md).

### Examples

```mathematica
Solve3[{2 x == b - w/2, y - d == p}, {x, y}]
```

$$\left\{x\to \frac{1}{4} (2 b-w),y\to d+p\right\}$$