```mathematica
 
```

## FCDiffEqSolve

`FCDiffEqSolve[mat, var, eps, n]` constructs a solution for a single-variable differential equation $G' = \varepsilon \mathcal{B} G$ in the canonical form, where `mat` is $B$, `var` is the variable w.r.t. which $G$ was differentiated and `n` is the required order in `eps`.

The output consists of iterated integrals written in terms of `FCIteratedIntegral` objects.

### See also

[Overview](Extra/FeynCalc.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCDiffEqChangeVariables](FCDiffEqChangeVariables.md)

### Examples

```mathematica
mat = {{-2/x, 0, 0}, {0, 0, 0}, {-x^(-1), 3/x, -2/x}}
```

$$\left(
\begin{array}{ccc}
 -\frac{2}{x} & 0 & 0 \\
 0 & 0 & 0 \\
 -\frac{1}{x} & \frac{3}{x} & -\frac{2}{x} \\
\end{array}
\right)$$

```mathematica
FCDiffEqSolve[mat, x, ep, 1]
```

$$\left\{\text{ep} \left(C[1,0] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x,0,x\right)+C[1,1]\right)+C[1,0],\text{ep} C[2,1]+C[2,0],\text{ep} \left(C[3,0] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -2 \\
\end{array}
\right),x\right),x,0,x\right)+C[1,0] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & -1 \\
\end{array}
\right),x\right),x,0,x\right)+C[2,0] \;\text{FCIteratedIntegral}\left(\text{FCPartialFractionForm}\left(0,\left(
\begin{array}{cc}
 \{x,-1\} & 3 \\
\end{array}
\right),x\right),x,0,x\right)+C[3,1]\right)+C[3,0]\right\}$$