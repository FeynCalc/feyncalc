## Solve2

`Solve2` is equivalent to `Solve`, except that it works only for linear equations (and returns just a list) and accepts the options `Factoring` and `FinalSubstitutions`.

`Solve2` uses the "high school algorithm" and factors intermediate results. Therefore it can be drastically more useful than `Solve`.

### See also

[Overview](Extra/FeynCalc.md), [Solve3](Solve3.md).

### Examples

```mathematica
Solve2[{2 x == b - w/2, y - d == p}, {x, y}]
```

$$\left\{x\to \frac{1}{4} (2 b-w),y\to d+p\right\}$$

If no equation sign is given the polynomials are supposed to be $0$.

```mathematica
Solve2[x + y, x]
```

$$\{x\to -y\}$$

```mathematica
Solve2[x + y, x, FinalSubstitutions -> {y -> h}]
```

$$\{x\to -h\}$$

```mathematica
Solve2[{2 x == b - w/2, y - d == p}, {x, y}, Factoring -> Expand]
```

$$\left\{x\to \frac{b}{2}-\frac{w}{4},y\to d+p\right\}$$

```mathematica
Solve[{2 x == b - w/2, y - d == p}, {x, y}]
```

$$\left\{\left\{x\to \frac{1}{4} (2 b-w),y\to d+p\right\}\right\}$$