## FCDiffEqChangeVariables

`FCDiffEqChangeVariables[mat, x, y, rule, yOfX]` applies a variable transformation from `x` to `y`described by `rule`, where `yOfX` denotes $y(x)$. Here `mat` is a matrix in the context of differential equations, i.e. it can be either the matrix $\mathcal{A}$ or $\mathcal{B}$ from the pre-canonical $F' = \mathcal{A} F$ or canonical $G' = \varepsilon \mathcal{B} G$  form, or the transformation matrix $\mathcal{T}$ with $F = \mathcal{T} G$ .

By default, the transformation also includes the prefactor $1/f'(y)$. This is correct for $\mathcal{A}$ or $\mathcal{B}$ but not for $\mathcal{T}$ matrices. The inclusion of the prefactor can be disabled by setting the option `Prefactor` to `False`.

### See also

[Overview](Extra/FeynCalc.md).

### Examples

```mathematica
mat = {{(-2*(-1 + eps))/x, 0, 0, 0}, {0, (1 - eps)/x, 0, 0}, {0, (-2*(-1 + eps))/(x*(-1 + 4*x)), 
    (-2*(-1 + 2*eps))/(-1 + 4*x), 0},  {(-2*(-1 + eps))/(x*(-1 + 4*x)), 0, 0, 
    (-1 + eps + 6*x - 8*eps*x)/(x*(-1 + 4*x))}}
```

$$\left(
\begin{array}{cccc}
 -\frac{2 (\text{eps}-1)}{x} & 0 & 0 & 0 \\
 0 & \frac{1-\text{eps}}{x} & 0 & 0 \\
 0 & -\frac{2 (\text{eps}-1)}{x (4 x-1)} & -\frac{2 (2 \;\text{eps}-1)}{4 x-1} & 0 \\
 -\frac{2 (\text{eps}-1)}{x (4 x-1)} & 0 & 0 & \frac{-8 \;\text{eps} x+\text{eps}+6 x-1}{x (4 x-1)} \\
\end{array}
\right)$$

```mathematica
matNew = FCDiffEqChangeVariables[mat, x, y, x -> (1 - y^2)/4, Sqrt[1 - 4*x], Assumptions -> {y > 0}]
```

$$\left(
\begin{array}{cccc}
 -\frac{4 (\text{eps}-1) y}{y^2-1} & 0 & 0 & 0 \\
 0 & -\frac{2 (\text{eps}-1) y}{y^2-1} & 0 & 0 \\
 0 & \frac{4-4 \;\text{eps}}{y-y^3} & \frac{1-2 \;\text{eps}}{y} & 0 \\
 \frac{4-4 \;\text{eps}}{y-y^3} & 0 & 0 & \frac{4 \;\text{eps} y^2-2 \;\text{eps}-3 y^2+1}{y-y^3} \\
\end{array}
\right)$$

Setting the option `Reverse` to `True` allows to undo the transformation.

```mathematica
matCheck = FCDiffEqChangeVariables[matNew, x, y, x -> (1 - y^2)/4, Sqrt[1 - 4*x], Reverse -> True]
```

$$\left(
\begin{array}{cccc}
 \frac{2-2 \;\text{eps}}{x} & 0 & 0 & 0 \\
 0 & \frac{1-\text{eps}}{x} & 0 & 0 \\
 0 & -\frac{2-2 \;\text{eps}}{x-4 x^2} & \frac{2-4 \;\text{eps}}{4 x-1} & 0 \\
 -\frac{2-2 \;\text{eps}}{x-4 x^2} & 0 & 0 & \frac{8 \;\text{eps} x-\text{eps}-6 x+1}{x-4 x^2} \\
\end{array}
\right)$$

```mathematica
Simplify[matCheck - mat] // Flatten // Union
```

$$\{0\}$$

```mathematica
FCDiffEqChangeVariables[mat, x, y, x -> (1 - y^2)/4, Sqrt[1 - 4*x], Assumptions -> {y > 0}, 
  Prefactor -> False]
```

$$\left(
\begin{array}{cccc}
 \frac{8 (\text{eps}-1)}{y^2-1} & 0 & 0 & 0 \\
 0 & \frac{4 (\text{eps}-1)}{y^2-1} & 0 & 0 \\
 0 & -\frac{8 (\text{eps}-1)}{y^2 \left(y^2-1\right)} & \frac{4 \;\text{eps}-2}{y^2} & 0 \\
 -\frac{8 (\text{eps}-1)}{y^2 \left(y^2-1\right)} & 0 & 0 & \frac{\text{eps} \left(8 y^2-4\right)-6 y^2+2}{y^2 \left(y^2-1\right)} \\
\end{array}
\right)$$