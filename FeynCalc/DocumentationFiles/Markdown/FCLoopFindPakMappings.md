`FCLoopFindPakMappings[{int1, int2, ...}, {p1, p2, ...}]` finds mappings between scalar multiloop-integrals `int1, int2, ...` that depend on the loop momenta `p1, p2, ...` using the algorithm of Alexey Pak (arXiv:1111.0868).

The current implementation is based on the `FindEquivalents` function from FIRE 6 (arXiv:1901.07808)

### See also

[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakOrder](FCLoopPakOrder).

### Examples

```mathematica
ints = {FAD[{p1, m1}], FAD[{p1 + q, m1}], FAD[{p1, m2}]}
```

$$\left\{\frac{1}{\text{p1}^2-\text{m1}^2},\frac{1}{(\text{p1}+q)^2-\text{m1}^2},\frac{1}{\text{p1}^2-\text{m2}^2}\right\}$$

```mathematica
FCLoopFindPakMappings[ints, {p1}]
```

$$\left\{\left\{\frac{1}{\text{p1}^2-\text{m1}^2},\frac{1}{(\text{p1}+q)^2-\text{m1}^2}\right\},\left\{\frac{1}{\text{p1}^2-\text{m2}^2}\right\}\right\}$$

```mathematica
ints = {FAD[p1] FAD[p1 - p3 - p4] FAD[p4] FAD[p3 + q1] FAD[{p3, m1}] FAD[{p1 - p4, m1}] FAD[{p1 + q1, 0}, {p1 + q1, 0}], 
   FAD[p4] FAD[p1 - p3 + q1] FAD[p3 + q1] FAD[p1 + p4 + q1] FAD[{p3, m1}] FAD[{p1 + q1, m1}] FAD[{p1 + p4 + 2 q1, 0}, {p1 + p4 + 2 q1, 0}], 
   FAD[p1] FAD[p4 - 2 q1] FAD[p3 + q1] FAD[p1 - p3 - p4 + 2 q1] FAD[{p3, m1}] FAD[{p1 - p4 + 2 q1, m1}] FAD[{p1 + q1, 0}, {p1 + q1, 0}]}
```

$$\left\{\frac{1}{\text{p1}^2 \text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^2^2 (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}-\text{p4})^2 \left((\text{p1}-\text{p4})^2-\text{m1}^2\right)},\frac{1}{\text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}+\text{q1})^2 (\text{p1}+\text{p4}+\text{q1})^2 (\text{p1}+\text{p4}+2 \text{q1})^2^2 \left((\text{p1}+\text{q1})^2-\text{m1}^2\right)},\frac{1}{\text{p1}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^2^2 (\text{p3}+\text{q1})^2 (\text{p4}-2 \text{q1})^2 (\text{p1}-\text{p3}-\text{p4}+2 \text{q1})^2 \left((\text{p1}-\text{p4}+2 \text{q1})^2-\text{m1}^2\right)}\right\}$$

```mathematica
FCLoopFindPakMappings[ints, {p1, p3, p4}, FCE -> True]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{\text{p1}^2 \text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^2^2 (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}-\text{p4})^2 \left((\text{p1}-\text{p4})^2-\text{m1}^2\right)} & \frac{1}{\text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}+\text{q1})^2 (\text{p1}+\text{p4}+\text{q1})^2 (\text{p1}+\text{p4}+2 \text{q1})^2^2 \left((\text{p1}+\text{q1})^2-\text{m1}^2\right)} & \frac{1}{\text{p1}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^2^2 (\text{p3}+\text{q1})^2 (\text{p4}-2 \text{q1})^2 (\text{p1}-\text{p3}-\text{p4}+2 \text{q1})^2 \left((\text{p1}-\text{p4}+2 \text{q1})^2-\text{m1}^2\right)} \\
\end{array}
\right)$$

```mathematica
FAD[q, q - p]
FCLoopIBPReducableQ[FCI[%]]
```

$$\frac{1}{q^2.(q-p)^2}$$

$$\text{False}$$

```mathematica
FAD[{q, 0, 2}, q - p]
FCLoopIBPReducableQ[FCI[%]]
```

$$\frac{1}{\left(q^2\right)^2.(q-p)^2}$$

$$\text{True}$$