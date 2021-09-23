## GenPaVe

`GenPaVe[i, j, ..., {{0, m0}, {Momentum[p1], m1}, {Momentum[p2], m2}, ...]` denotes the invariant (and scalar) Passarino-Veltman integrals, i.e. the coefficient functions of the tensor integral decomposition. In contrast to `PaVe` which uses the LoopTools convention,  masses and external momenta in `GenPaVe` are written in the same order as they appear in the original tensor integral, i.e. `FAD[{q,m0},{q-p1,m1},{q-p2,m2},...]`.

### See also

[Overview](Extra/FeynCalc.md), [PaVe](PaVe.md).

### Examples

```mathematica
FVD[q, \[Mu]] FVD[q, \[Nu]] FAD[{q, m0}, {q + p1, m1}, {q + p2, m2}]/(I*Pi^2)
TID[%, q, UsePaVeBasis -> True]
TID[%%, q, UsePaVeBasis -> True, GenPaVe -> True]
```

$$-\frac{i q^{\mu } q^{\nu }}{\pi ^2 \left(q^2-\text{m0}^2\right).\left((\text{p1}+q)^2-\text{m1}^2\right).\left((\text{p2}+q)^2-\text{m2}^2\right)}$$

$$g^{\mu \nu } \;\text{C}_{00}\left(\text{p1}^2,\text{p2}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{m1}^2,\text{m0}^2,\text{m2}^2\right)+\text{p1}^{\mu } \;\text{p1}^{\nu } \;\text{C}_{11}\left(\text{p1}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{p2}^2,\text{m0}^2,\text{m1}^2,\text{m2}^2\right)+\text{p2}^{\mu } \;\text{p2}^{\nu } \;\text{C}_{11}\left(\text{p2}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{p1}^2,\text{m0}^2,\text{m2}^2,\text{m1}^2\right)+\left(\text{p1}^{\nu } \;\text{p2}^{\mu }+\text{p1}^{\mu } \;\text{p2}^{\nu }\right) \;\text{C}_{12}\left(\text{p1}^2,-2 (\text{p1}\cdot \;\text{p2})+\text{p1}^2+\text{p2}^2,\text{p2}^2,\text{m0}^2,\text{m1}^2,\text{m2}^2\right)$$

$$g^{\mu \nu } \;\text{GenPaVe}\left(\{0,0\},\left(
\begin{array}{cc}
 0 & \;\text{m0} \\
 \;\text{p1} & \;\text{m1} \\
 \;\text{p2} & \;\text{m2} \\
\end{array}
\right)\right)+\text{p1}^{\mu } \;\text{p1}^{\nu } \;\text{GenPaVe}\left(\{1,1\},\left(
\begin{array}{cc}
 0 & \;\text{m0} \\
 \;\text{p1} & \;\text{m1} \\
 \;\text{p2} & \;\text{m2} \\
\end{array}
\right)\right)+\text{p2}^{\mu } \;\text{p2}^{\nu } \;\text{GenPaVe}\left(\{2,2\},\left(
\begin{array}{cc}
 0 & \;\text{m0} \\
 \;\text{p1} & \;\text{m1} \\
 \;\text{p2} & \;\text{m2} \\
\end{array}
\right)\right)+\left(\text{p1}^{\nu } \;\text{p2}^{\mu }+\text{p1}^{\mu } \;\text{p2}^{\nu }\right) \;\text{GenPaVe}\left(\{1,2\},\left(
\begin{array}{cc}
 0 & \;\text{m0} \\
 \;\text{p1} & \;\text{m1} \\
 \;\text{p2} & \;\text{m2} \\
\end{array}
\right)\right)$$
