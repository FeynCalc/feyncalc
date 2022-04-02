## FactorList2

`FactorList2[exp]` is similar to `FactorList` except that it correctly handles symbolic exponents.

### See also

[Overview](Extra/FeynCalc.md), [Factor2](Factor2.md).

### Examples

```mathematica
FactorList2[(x[1] x[2] + x[1] x[3] + x[2] x[3])^(-3 + 3 ep)/(x[1]^2 x[2] + x[1]^2 x[3])^(-1 + 2 ep)]
```

$$\left(
\begin{array}{cc}
 1 & 1 \\
 x(1)^2 (x(2)+x(3)) & -2 \text{ep} \\
 x(1) x(2)+x(3) x(2)+x(1) x(3) & 3 (\text{ep}-1) \\
 x(1) & 2 \\
 x(2)+x(3) & 1 \\
\end{array}
\right)$$