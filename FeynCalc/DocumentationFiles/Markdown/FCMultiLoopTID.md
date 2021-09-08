## FCMultiLoopTID

`FCMultiLoopTID[amp, {q1, q2, ...}]` does a multi-loop tensor integral decomposition, transforming the Lorentz indices away from the loop momenta `q1, q2, ...` The decomposition is applied only to the loop integrals where loop momenta are contracted with Dirac matrices or epsilon tensors.

### See also

[Overview](Extra/FeynCalc.md), [TID](TID.md).

### Examples

```mathematica
FCI[FVD[q1, \[Mu]] FVD[q2, \[Nu]] FAD[q1, q2, {q1 - p1}, {q2 - p1}, {q1 - q2}]]
FCMultiLoopTID[%, {q1, q2}]
```

$$\frac{\text{q1}^{\mu } \;\text{q2}^{\nu }}{\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q2}-\text{p1})^2.(\text{q1}-\text{q2})^2}$$

$$\frac{\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{(1-D) \;\text{p1}^2 \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q1}-\text{q2})^2}-\frac{\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{2 (1-D) \;\text{p1}^2 \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q2}-\text{p1})^2}-\frac{D \;\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{4 (1-D) \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{p1})^2.(\text{q1}-\text{q2})^2.(\text{q2}-\text{p1})^2}+\frac{D \;\text{p1}^{\mu } \;\text{p1}^{\nu }-\text{p1}^2 g^{\mu \nu }}{2 (1-D) \;\text{p1}^4 \;\text{q1}^2.(\text{q1}-\text{q2})^2.(\text{q2}-\text{p1})^2}$$
