## FCGramMatrix

`FCGramMatrix[{p1, p2, ...}]` creates a Gram matrix from the given list of momenta.

### See also

[Overview](Extra/FeynCalc.md), [FCGramDeterminant](FCGramDeterminant.md).

### Examples

```mathematica
FCGramMatrix[{p1, p2}]
```

$$\left(
\begin{array}{cc}
 2 \;\text{p1}^2 & 2 (\text{p1}\cdot \;\text{p2}) \\
 2 (\text{p1}\cdot \;\text{p2}) & 2 \;\text{p2}^2 \\
\end{array}
\right)$$

```mathematica
FCGramMatrix[{p1, p2, p3}]
```

$$\left(
\begin{array}{ccc}
 2 \;\text{p1}^2 & 2 (\text{p1}\cdot \;\text{p2}) & 2 (\text{p1}\cdot \;\text{p3}) \\
 2 (\text{p1}\cdot \;\text{p2}) & 2 \;\text{p2}^2 & 2 (\text{p2}\cdot \;\text{p3}) \\
 2 (\text{p1}\cdot \;\text{p3}) & 2 (\text{p2}\cdot \;\text{p3}) & 2 \;\text{p3}^2 \\
\end{array}
\right)$$

```mathematica
FCGramMatrix[{p1, p2, p3}, Head -> {CartesianPair, CartesianMomentum},Dimension -> D - 1]
Det[%]
```

$$\left(
\begin{array}{ccc}
 2 \;\text{p1}^2 & 2 (\text{p1}\cdot \;\text{p2}) & 2 (\text{p1}\cdot \;\text{p3}) \\
 2 (\text{p1}\cdot \;\text{p2}) & 2 \;\text{p2}^2 & 2 (\text{p2}\cdot \;\text{p3}) \\
 2 (\text{p1}\cdot \;\text{p3}) & 2 (\text{p2}\cdot \;\text{p3}) & 2 \;\text{p3}^2 \\
\end{array}
\right)$$

$$-8 \;\text{p3}^2 (\text{p1}\cdot \;\text{p2})^2-8 \;\text{p1}^2 (\text{p2}\cdot \;\text{p3})^2-8 \;\text{p2}^2 (\text{p1}\cdot \;\text{p3})^2+8 \;\text{p1}^2 \;\text{p2}^2 \;\text{p3}^2+16 (\text{p1}\cdot \;\text{p2}) (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})$$

```mathematica
FCGramDeterminant[{p1, p2, p3}, Head -> {CartesianPair, CartesianMomentum}, Dimension -> D - 1]
```

$$-8 \;\text{p3}^2 (\text{p1}\cdot \;\text{p2})^2-8 \;\text{p1}^2 (\text{p2}\cdot \;\text{p3})^2-8 \;\text{p2}^2 (\text{p1}\cdot \;\text{p3})^2+8 \;\text{p1}^2 \;\text{p2}^2 \;\text{p3}^2+16 (\text{p1}\cdot \;\text{p2}) (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})$$
