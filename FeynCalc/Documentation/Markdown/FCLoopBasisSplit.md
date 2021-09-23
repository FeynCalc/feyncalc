## FCLoopBasisSplit

`FCLoopBasisSplit[int, {q1, q2, ...}]` checks if the given loop integral factorizes and if so splits it into independent integrals.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCI@FAD[{q1, m}, {q2, m}, {p1 - p2, 0}]
FCLoopBasisSplit[%, {q1, q2}, Head -> loopInt]
```

$$\frac{1}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{p1}-\text{p2})^2}$$

$$\left\{\text{loopInt}\left(\frac{1}{\text{q1}^2-m^2},\{\text{q1}\}\right),\text{loopInt}\left(\frac{1}{\text{q2}^2-m^2},\{\text{q2}\}\right),\text{loopInt}\left(\frac{1}{(\text{p1}-\text{p2})^2},0\right)\right\}$$

```mathematica
FCI[SFAD[q1, q1 - q2, q2, {q3, m^2}]]
FCLoopBasisSplit[%, {q1, q2, q3}, Head -> loop, FCE -> True]
```

$$\frac{1}{(\text{q1}^2+i \eta ).((\text{q1}-\text{q2})^2+i \eta ).(\text{q2}^2+i \eta ).(\text{q3}^2-m^2+i \eta )}$$

$$\left\{\text{loop}\left(\frac{1}{(\text{q3}^2-m^2+i \eta )},\{\text{q3}\}\right),\text{loop}\left(\frac{1}{(\text{q1}^2+i \eta ).(\text{q2}^2+i \eta ).((\text{q1}-\text{q2})^2+i \eta )},\{\text{q1},\text{q2}\}\right)\right\}$$
