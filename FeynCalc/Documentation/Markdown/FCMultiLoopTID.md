## FCMultiLoopTID

`FCMultiLoopTID[amp, {q1, q2, ...}]` does a multi-loop tensor integral decomposition, transforming the Lorentz indices away from the loop momenta `q1, q2, ...` The decomposition is applied only to the loop integrals where loop momenta are contracted with Dirac matrices or epsilon tensors.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFindTensorBasis](FCLoopFindTensorBasis.md), [TID](TID.md).

### Examples

```mathematica
FCI[FVD[q1, \[Mu]] FVD[q2, \[Nu]] FAD[q1, q2, {q1 - p1}, {q2 - p1}, {q1 - q2}]] 
 
FCMultiLoopTID[%, {q1, q2}]
```

$$\frac{\text{q1}^{\mu } \;\text{q2}^{\nu }}{\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q2}-\text{p1})^2.(\text{q1}-\text{q2})^2}$$

$$\frac{\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{(1-D) \;\text{p1}^2 \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q1}-\text{q2})^2}-\frac{\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{2 (1-D) \;\text{p1}^2 \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q2}-\text{p1})^2}-\frac{D \;\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{4 (1-D) \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q1}-\text{q2})^2.(\text{q2}-\text{p1})^2}+\frac{D \;\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{2 (1-D) \;\text{p1}^4 \;\text{q1}^2.(\text{q1}-\text{q2})^2.(\text{q2}-\text{p1})^2}$$

In the case of vanishing Gram determinants one can apply the same procedure as in the case of TID or FCLoopTensorReduce: one uses `FCLoopFindTensorBasis` to find a linear independent basis of external momenta and then supplies this basis to the function.

```mathematica
FCClearScalarProducts[]
SPD[p1] = m1^2;
SPD[p2] = m2^2;
SPD[p1, p2] = m1 m2;
```

```mathematica
FCMultiLoopTID[FVD[q1, mu] FAD[{q1, m}, {q1 + p1}, {q1 + p2}], {q1}]
```

![0h2gltw65l2pe](img/0h2gltw65l2pe.svg)

$$\text{\$Aborted}$$

```mathematica
FCLoopFindTensorBasis[{p1, p2}, {}, n]
```

$$\left(
\begin{array}{c}
 \;\text{p1} \\
 \;\text{p2} \\
 \;\text{p2}\to \;\text{p1} \;\text{FCGV}(\text{Prefactor})\left(\frac{\text{m2}}{\text{m1}}\right) \\
\end{array}
\right)$$

```mathematica
FCMultiLoopTID[FVD[q1, mu] FAD[{q1, m}, {q1 + p1}, {q1 + p2}], {q1}, 
  TensorReductionBasisChange -> {{p1, p2} -> {p1}}]
```

$$\frac{\text{p1}^{\text{mu}}}{2 \;\text{m1}^2 \;\text{q1}^2.\left((\text{q1}-\text{p2})^2-m^2\right)}-\frac{\left(m^2+\text{m1}^2\right) \;\text{p1}^{\text{mu}}}{2 \;\text{m1}^2 \left(\text{q1}^2-m^2\right).(\text{q1}-\text{p1})^2.(\text{q1}-\text{p2})^2}-\frac{\text{p1}^{\text{mu}}}{2 \;\text{m1}^2 \;\text{q1}^2.(-\text{p1}+\text{p2}+\text{q1})^2}$$