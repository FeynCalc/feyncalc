## Combine

`Combine[expr]` puts terms in a sum over a common denominator and cancels factors in the result. `Combine` is similar to `Together`, but accepts the option `Expanding` and works usually better than `Together` for polynomials involving rationals with sums in the denominator.

### See also

[Overview](Extra/FeynCalc.md), [Factor2](Factor2.md).

### Examples

```mathematica
Combine[((a - b) (c - d))/e + g]
```

$$\frac{(a-b) (c-d)+e g}{e}$$

Here the result from `Together` where the numerator is automatically expanded.

```mathematica
Together[((a - b) (c - d))/e + g]
```

$$\frac{a c-a d-b c+b d+e g}{e}$$

If the option `Expanding` is set to `True`, the result of `Combine` is the same as `Together`, but uses a slightly different algorithm.

```mathematica
Combine[((a - b) (c - d))/e + g, Expanding -> True]
```

$$\frac{a c-a d-b c+b d+e g}{e}$$
