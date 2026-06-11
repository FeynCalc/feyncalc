## FCLoopFactorizingQ

`FCLoopFactorizinQI[int, topo]` checks whether the given loop integral factorizes or not. The input can be made integrals in the `GLI` or `FAD` notation.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFactorizingSplit](FCLoopFactorizingSplit.md), [FCLoopCreateFactorizingRules](FCLoopCreateFactorizingRules.md).

### Examples

```mathematica
FCLoopFactorizingQ[FAD[{k, m}], {k}]
```

$$\text{False}$$

```mathematica
FCLoopFactorizingQ[FAD[{k1, m1}, {k2, m2}, {k1 - k2}], {k1, k2}]
```

$$\text{False}$$

```mathematica
FCLoopFactorizingQ[FAD[{k1, m1}, {k2, m2}], {k1, k2}]
```

$$\text{True}$$

```mathematica
int = FAD[{k1, m1}, {k1 - p1}, {k2, m2}, {k2 - p2}] /. k1 -> k1 + k2
```

$$\frac{1}{\left((\text{k1}+\text{k2})^2-\text{m1}^2\right).(\text{k1}+\text{k2}-\text{p1})^2.\left(\text{k2}^2-\text{m2}^2\right).(\text{k2}-\text{p2})^2}$$

```mathematica
FCLoopFactorizingQ[int, {k1, k2}]
```

$$\text{True}$$