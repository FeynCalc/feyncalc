## FCLoopFactorizingSplit

`FCLoopFactorizingSplit[int, topo]` checks whether the given loop integral factorizes and separates it into factorizing integrals if it is the case. The input can be made integrals in the `GLI` or `FAD` notation.

Notice that the output is always given in the `FAD` notation even if the input was provided using `GLI`s.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFactorizingQ](FCLoopFactorizingQ.md), [FCLoopCreateFactorizingRules](FCLoopCreateFactorizingRules.md).

### Examples

```mathematica
FCReloadFunctionFromFile[FCLoopFactorizingSplit]
```

```mathematica
FCLoopFactorizingSplit[{FAD[{k, m}]}, {k}]
```

$$\left(
\begin{array}{c}
 \left\{\frac{1}{k^2-m^2},\{k\},\{\}\right\} \\
\end{array}
\right)$$

```mathematica
FCLoopFactorizingSplit[FAD[{k1, m1}, {k2, m2}, {k1 - k2}], {k1, k2}]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{\left(\text{k1}^2-\text{m1}^2\right) \left(\text{k2}^2-\text{m2}^2\right) (\text{k1}-\text{k2})^2} & \{\text{k1},\text{k2}\} & \{\} \\
\end{array}
\right)$$

```mathematica
FCLoopFactorizingSplit[FAD[{k1, m1}, {k2, m2}], {k1, k2}]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{\text{k1}^2-\text{m1}^2} & \{\text{k1}\} & \{\} \\
 \frac{1}{\text{k2}^2-\text{m2}^2} & \{\text{k2}\} & \{\} \\
\end{array}
\right)$$

```mathematica
int = FAD[{k1, m1}, {k1 - p1}, {k2, m2}, {k2 - p2}] /. k1 -> k1 + k2
```

$$\frac{1}{\left((\text{k1}+\text{k2})^2-\text{m1}^2\right).(\text{k1}+\text{k2}-\text{p1})^2.\left(\text{k2}^2-\text{m2}^2\right).(\text{k2}-\text{p2})^2}$$

```mathematica
FCLoopFactorizingSplit[int, {k1, k2}]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{\left(\text{k1}^2-\text{m1}^2\right) (\text{k1}-\text{p1})^2} & \{\text{k1}\} & \{\} \\
 \frac{1}{\left(\text{k2}^2-\text{m2}^2\right) (\text{k2}-\text{p2})^2} & \{\text{k2}\} & \{\} \\
\end{array}
\right)$$