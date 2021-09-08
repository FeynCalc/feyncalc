## FCLoopBasisExtract

`FCLoopBasisExtract[int, {q1, q2, ...}]` is an auxiliary function that extracts the scalar products that form the basis of the loop integral in int. It needs to know the loop momenta on which the integral depends and the dimensions of the momenta that may occur in the integral.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
SPD[q, p] SFAD[q, q - p, q - p]
FCLoopBasisExtract[%, {q}, SetDimensions -> {4, D}]
```

$$\frac{p\cdot q}{(q^2+i \eta ).((q-p)^2+i \eta )^2}$$

$$\left\{\left\{p\cdot q,q^2,-2 (p\cdot q)+p^2+q^2\right\},\left\{p\cdot q,q^2\right\},\{-1,1,2\},\left\{p\cdot q,\frac{1}{(q^2+i \eta )},\frac{1}{((q-p)^2+i \eta )}\right\}\right\}$$

```mathematica
SFAD[p1]
FCLoopBasisExtract[%, {p1, p2, p3}, FCTopology -> True, FCE -> True]
```

$$\frac{1}{(\text{p1}^2+i \eta )}$$

$$\left\{\left\{\text{p1}^2\right\},\left\{\text{p1}^2,\text{p1}\cdot \;\text{p2},\text{p1}\cdot \;\text{p3},\text{p2}^2,\text{p2}\cdot \;\text{p3},\text{p3}^2\right\},\{1\},\left\{\frac{1}{(\text{p1}^2+i \eta )}\right\}\right\}$$
