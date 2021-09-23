## Series3

`Series3` performs a series expansion around `0`. `Series3` is equivalent to `Series`, except that it applies `Normal` on the result and that some `Series` bugs are fixed.

`Series3[f, e, n]` is equivalent to  `Series3[f, {e, 0, n}]`.

### See also

[Overview](Extra/FeynCalc.md), [Series2](Series2.md).

### Examples

```mathematica
Series3[(x (1 - x))^(\[Delta]/2), \[Delta], 1]
```

$$\frac{1}{2} \delta  \log ((1-x) x)+1$$

```mathematica
Series3[Gamma[x], x, 1] // FullSimplify
```

$$\frac{1}{x}-\gamma +1$$
