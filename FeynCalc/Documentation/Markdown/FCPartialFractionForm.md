## FCPartialFractionForm

`FCPartialFractionForm[n, {{f1,x-r1,p1},{f2,x-r2,p2}, ...}, x]` is a special way of representing sums of rational functions of `x` given by $n + \frac{f_1}{[x-r_1]^p_1} + \frac{f_2}{[x-r_2]^p_2} + \ldots$

It is inspired by the `parfrac`form from Maple and its usage in E. Panzer's HyperInt for the integration of multiple polylogarithms.

Use `ToFCPartialFractionForm` to convert the given expression to this notation and `FromFCPartialFractionForm` to return back to the usual representation.

### See also

[Overview](Extra/FeynCalc.md), [ToFCPartialFractionForm](ToFCPartialFractionForm.md), [FromFCPartialFractionForm](FromFCPartialFractionForm.md).

### Examples

```mathematica
Apart[c + x^2/(x - 1), x]
```

$$c+x+\frac{1}{x-1}+1$$

```mathematica
ex1 = ToFCPartialFractionForm[c + x^2/(x - 1), x]
```

$$\text{FCPartialFractionForm}\left(c+x+1,\left(
\begin{array}{cc}
 \{x-1,-1\} & 1 \\
\end{array}
\right),x\right)$$

```mathematica
FromFCPartialFractionForm[ex1]
```

$$c+x+\frac{1}{x-1}+1$$

```mathematica
ex2 = ToFCPartialFractionForm[(-64*(-1 + z^2))/(15*(1 + z^2 + z^4)), z]
```

$$\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \left\{z-\sqrt[3]{-1},-1\right\} & -\frac{32}{15} \\
 \left\{z+\sqrt[3]{-1},-1\right\} & \frac{32}{15} \\
 \left\{z-(-1)^{2/3},-1\right\} & \frac{32}{15} \\
 \left\{z+(-1)^{2/3},-1\right\} & -\frac{32}{15} \\
\end{array}
\right),z\right)$$

```mathematica
FromFCPartialFractionForm[ex2]
```

$$\frac{32}{15 \left(z+\sqrt[3]{-1}\right)}+\frac{32}{15 \left(z-(-1)^{2/3}\right)}-\frac{32}{15 \left(z+(-1)^{2/3}\right)}-\frac{32}{15 \left(z-\sqrt[3]{-1}\right)}$$

```mathematica
FromFCPartialFractionForm[ex2, Factoring -> Simplify]
```

$$-\frac{64 \left(z^2-1\right)}{15 \left(z^4+z^2+1\right)}$$