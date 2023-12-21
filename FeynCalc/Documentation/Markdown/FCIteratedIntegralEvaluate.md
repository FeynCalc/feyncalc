```mathematica
 
```

## FCIteratedIntegralEvaluate

`FCIteratedIntegralEvaluate[ex]` evaluates iterated integrals in ex in terms of multiple polylogarithms.

To that aim the `ex` must contain ration functions (in the `FCPartialFractionForm` notation) and possibly `FCGPL`s wrapped with `FCIteratedIntegral` heads

### See also

[Overview](Extra/FeynCalc.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCIteratedIntegralSimplify](FCIteratedIntegralSimplify.md), [FCGPL](FCGPL.md).

### Examples

```mathematica
int = FCPartialFractionForm[0, {{{-a + x[2], -1}, (1 + a + x[3])^(-2)}, 
    {{1 + x[2] + x[3], -2}, -(1 + a + x[3])^(-1)}, {{1 + x[2] + x[3], -1}, -(1 + a + x[3])^(-2)}}, x[2]]
```

$$\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x(2)-a,-1\} & \frac{1}{(a+x(3)+1)^2} \\
 \{x(2)+x(3)+1,-2\} & -\frac{1}{a+x(3)+1} \\
 \{x(2)+x(3)+1,-1\} & -\frac{1}{(a+x(3)+1)^2} \\
\end{array}
\right),x(2)\right)$$

```mathematica
FCIteratedIntegralEvaluate[FCIteratedIntegral[int, x[2], 0, Infinity]]
```

$$-\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{\infty ,-1\} & -\frac{1}{a+x(3)+1} \\
\end{array}
\right),\infty \right)+\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x(3)+1,-1\} & -\frac{1}{a+x(3)+1} \\
\end{array}
\right),0\right)-\frac{G(-x[3]-1; \infty )}{(a+x(3)+1)^2}+\frac{G(a; \infty )}{(a+x(3)+1)^2}$$

```mathematica
FCIteratedIntegralEvaluate[FCIteratedIntegral[int, x[2], 0, x[2]]]
```

$$\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x(3)+1,-1\} & -\frac{1}{a+x(3)+1} \\
\end{array}
\right),0\right)-\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x(2)+x(3)+1,-1\} & -\frac{1}{a+x(3)+1} \\
\end{array}
\right),x(2)\right)+\frac{G(a; x[2])}{(a+x(3)+1)^2}-\frac{G(-x[3]-1; x[2])}{(a+x(3)+1)^2}$$