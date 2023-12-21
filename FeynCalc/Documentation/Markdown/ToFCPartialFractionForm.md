```mathematica
 
```

## ToFCPartialFractionForm

`ToFCPartialFractionForm[exp, x]` converts sums of rational functions of the form $n + \frac{f_1}{[x-r_1]^p_1} + \frac{f_2}{[x-r_2]^p_2} + \ldots$ to `FCPartialFractionForm[n, {{f1,x-r1,p1},{f2,x-r2,p2}, ...}, x]`.

This facilitates the handling of iterated integrals.

### See also

[Overview](Extra/FeynCalc.md), [FCPartialFractionForm](FCPartialFractionForm.md), [FromFCPartialFractionForm](FromFCPartialFractionForm.md).

### Examples

```mathematica
x/(x + 1) 
 
ToFCPartialFractionForm[%, x]
```

$$\frac{x}{x+1}$$

$$\text{FCPartialFractionForm}\left(1,\left(
\begin{array}{cc}
 \{x+1,-1\} & -1 \\
\end{array}
\right),x\right)$$

```mathematica
1/(x^2 + 3) 
 
ToFCPartialFractionForm[%, x]
```

$$\frac{1}{x^2+3}$$

$$\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \left\{x-i \sqrt{3},-1\right\} & -\frac{i}{2 \sqrt{3}} \\
 \left\{x+i \sqrt{3},-1\right\} & \frac{i}{2 \sqrt{3}} \\
\end{array}
\right),x\right)$$

```mathematica
(-64*(-1 + z^2))/(15*(1 + z^2 + z^4)) 
 
ToFCPartialFractionForm[%, z]

```

$$-\frac{64 \left(z^2-1\right)}{15 \left(z^4+z^2+1\right)}$$

$$\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \left\{z-\sqrt[3]{-1},-1\right\} & -\frac{32}{15} \\
 \left\{z+\sqrt[3]{-1},-1\right\} & \frac{32}{15} \\
 \left\{z-(-1)^{2/3},-1\right\} & \frac{32}{15} \\
 \left\{z+(-1)^{2/3},-1\right\} & -\frac{32}{15} \\
\end{array}
\right),z\right)$$