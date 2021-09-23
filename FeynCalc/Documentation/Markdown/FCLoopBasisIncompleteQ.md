## FCLoopBasisIncompleteQ

`FCLoopBasisIncompleteQ[int, {q1, q2, ...}]` checks whether the loop integral or topology `int` lacks propagators need to have a linearly independent basis .

The input can also consist of an `FCTopology` object or a list thereof.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopBasisOverdeterminedQ](FCLoopBasisOverdeterminedQ.md).

### Examples

```mathematica
FAD[{q1, m1}]
FCLoopBasisIncompleteQ[%, {q1}]
```

$$\frac{1}{\text{q1}^2-\text{m1}^2}$$

$$\text{False}$$

```mathematica
SPD[q1, l] FAD[{q1, m1}, {q1 - l + p, m}]
FCLoopBasisIncompleteQ[%, {q1}]
```

$$\frac{l\cdot \;\text{q1}}{\left(\text{q1}^2-\text{m1}^2\right).\left((-l+p+\text{q1})^2-m^2\right)}$$

$$\text{False}$$

```mathematica
FAD[{q1, m1}, {q2, m2}]
FCLoopBasisIncompleteQ[%, {q1, q2}]
```

$$\frac{1}{\left(\text{q1}^2-\text{m1}^2\right).\left(\text{q2}^2-\text{m2}^2\right)}$$

$$\text{True}$$

```mathematica
FAD[q1, q2, {q1 - l1, m1}, {q2 - l2, m2}]
FCLoopBasisIncompleteQ[%, {q1, q2}]
```

$$\frac{1}{\text{q1}^2.\text{q2}^2.\left((\text{q1}-\text{l1})^2-\text{m1}^2\right).\left((\text{q2}-\text{l2})^2-\text{m2}^2\right)}$$

$$\text{True}$$

```mathematica
CSPD[q1, l] CFAD[{q1, m1}, {q1 - l + p, m}]
FCLoopBasisIncompleteQ[%, {q1}]
```

$$\frac{l\cdot \;\text{q1}}{(\text{q1}^2+\text{m1}-i \eta ).((-l+p+\text{q1})^2+m-i \eta )}$$

$$\text{False}$$

```mathematica
SFAD[{q1, m1}, {q2, m2}]
FCLoopBasisIncompleteQ[%, {q1, q2}]
```

$$\frac{1}{(\text{q1}^2-\text{m1}+i \eta ).(\text{q2}^2-\text{m2}+i \eta )}$$

$$\text{True}$$

```mathematica
FCLoopBasisIncompleteQ[FCTopology[topo, {FAD[p1], 
    FAD[p2], FAD[p1 - q], FAD[p2 - q]}, {p1, p2}, {q}, {}, {}]]
```

$$\text{True}$$

```mathematica
FCLoopBasisIncompleteQ[{
   FCTopology[topo1, {FAD[p1], FAD[p2], FAD[p1 - q], FAD[p2 - q]}, {p1, p2}, {q}, {}, {}], 
   FCTopology[topo2, {FAD[p1], FAD[p2], FAD[p1 - q], FAD[p2 - p1]}, {p1, p2}, {q}, {}, {}], 
   FCTopology[topo3, {FAD[p1], FAD[p1 - q]}, {p1}, {q}, {}, {}] 
  }]
```

$$\{\text{True},\text{True},\text{False}\}$$
