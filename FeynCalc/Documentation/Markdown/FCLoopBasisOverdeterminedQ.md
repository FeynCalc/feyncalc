## FCLoopBasisOverdeterminedQ

`FCLoopBasisOverdeterminedQ[int, {q1, q2, ...}]` checks whether the loop integral or topology `int` contains linearly dependent propagators.

The input can also consist of an `FCTopology` object or a list thereof.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopBasisIncompleteQ](FCLoopBasisIncompleteQ.md).

### Examples

```mathematica
FAD[{q1, m1}, {q1 - l + p, m}]
FCLoopBasisOverdeterminedQ[%, {q1}]
```

$$\frac{1}{\left(\text{q1}^2-\text{m1}^2\right).\left((-l+p+\text{q1})^2-m^2\right)}$$

$$\text{False}$$

```mathematica
FAD[q1, {q1, m1}]
FCLoopBasisOverdeterminedQ[%, {q1}]
```

$$\frac{1}{\text{q1}^2.\left(\text{q1}^2-\text{m1}^2\right)}$$

$$\text{True}$$

```mathematica
FAD[q1, q2, {q1 + l, m1}, {q1 - l, m1}, {q2 + l, m1}, {q2 - l, m1}]
FCLoopBasisOverdeterminedQ[%, {q1, q2}]
```

$$\frac{1}{\text{q1}^2.\text{q2}^2.\left((l+\text{q1})^2-\text{m1}^2\right).\left((\text{q1}-l)^2-\text{m1}^2\right).\left((l+\text{q2})^2-\text{m1}^2\right).\left((\text{q2}-l)^2-\text{m1}^2\right)}$$

$$\text{True}$$

```mathematica
FCLoopBasisOverdeterminedQ[FCTopology[topo1, {FAD[p1], FAD[p2], 
    FAD[p1 - q], FAD[p2 - q], FAD[p1 - p2], FAD[p1 + p2 + q]}, {p1, p2}, {q}, {}, {}]]
```

$$\text{True}$$

```mathematica
FCLoopBasisOverdeterminedQ[{FCTopology[topo1, {FAD[p1], FAD[p2], 
     FAD[p1 - q], FAD[p2 - q], FAD[p1 - p2], FAD[p1 + p2 + q]}, {p1, p2}, {q}, {}, {}], 
   FCTopology[topo2, {FAD[p1], FAD[p2], 
     FAD[p1 - q], FAD[p2 - q], FAD[p1 - p2]}, {p1, p2}, {q}, {}, {}] 
  }]
```

$$\{\text{True},\text{False}\}$$
