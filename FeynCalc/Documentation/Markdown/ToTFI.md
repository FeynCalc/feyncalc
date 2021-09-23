## ToTFI

`ToTFI[expr, q1, q2, p]` translates FeynCalc 2-loop self energy type integrals into the `TFI` notation, which can be used as input for the function `TarcerRecurse` from the `TARCER` package. See the `TARCER` documenatation on `TFI` for details on the conventions.

### See also

[Overview](Extra/FeynCalc.md), [FromTFI](FromTFI.md).

### Examples

```mathematica
FAD[q1, q1 - p, {q2, M}, {q2 - p, m}, q1 - q2]
ToTFI[%, q1, q2, p]
% // StandardForm
```

$$\frac{1}{\text{q1}^2.(\text{q1}-p)^2.\left(\text{q2}^2-M^2\right).\left((\text{q2}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}$$

$$\text{TFI}\left(D,p^2,\left(
\begin{array}{cc}
 1 & 0 \\
 1 & M \\
 1 & 0 \\
 1 & m \\
 1 & 0 \\
\end{array}
\right)\right)$$

```
(*TFI[D, SPD[p, p], {{1, 0}, {1, M}, {1, 0}, {1, m}, {1, 0}}]*)
```

```mathematica
SOD[q1] SOD[q2] FAD[q1, q1 - p, {q2, M}, {q2 - p, m}, q1 - q2] // FCI
```

$$\frac{(\Delta \cdot \;\text{q1}) (\Delta \cdot \;\text{q2})}{\text{q1}^2.(\text{q1}-p)^2.\left(\text{q2}^2-M^2\right).\left((\text{q2}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}$$
