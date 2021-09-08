## FCLoopIsolate

`FCLoopIsolate[expr, {q1, q2, ...}]` wraps loop integrals into heads specified by the user. This is useful when you want to know which loop integrals appear in the given expression.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopExtract](FCLoopExtract.md).

### Examples

```mathematica
FCI[GSD[q - p1] . (GSD[q - p2] + M) . GSD[p3] SPD[q, p2] FAD[q, q - p1, {q - p2, m}]]
FCLoopIsolate[%, {q}, Head -> loopInt]
Cases2[%, loopInt]
```

$$\frac{(\text{p2}\cdot q) (\gamma \cdot (q-\text{p1})).(M+\gamma \cdot (q-\text{p2})).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}$$

$$((\gamma \cdot \;\text{p1}).(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p3})-M (\gamma \cdot \;\text{p1}).(\gamma \cdot \;\text{p3})) \;\text{loopInt}\left(\frac{\text{p2}\cdot q}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)+M \;\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)-\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot \;\text{p1}).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)-\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)+\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)$$

$$\left\{\text{loopInt}\left(\frac{\text{p2}\cdot q}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot \;\text{p1}).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)\right\}$$

```mathematica
TID[FVD[q, \[Mu]] FVD[q, \[Nu]] FAD[{q, m}, {q + p, m}, {q + r, m}], q, UsePaVeBasis -> True]
FCLoopIsolate[%, {q}, Head -> l]
Cases2[%, l]
```

$$i \pi ^2 g^{\mu \nu } \;\text{C}_{00}\left(p^2,r^2,-2 (p\cdot r)+p^2+r^2,m^2,m^2,m^2\right)+i \pi ^2 p^{\mu } p^{\nu } \;\text{C}_{11}\left(p^2,-2 (p\cdot r)+p^2+r^2,r^2,m^2,m^2,m^2\right)+i \pi ^2 r^{\mu } r^{\nu } \;\text{C}_{11}\left(r^2,-2 (p\cdot r)+p^2+r^2,p^2,m^2,m^2,m^2\right)+i \pi ^2 \left(p^{\nu } r^{\mu }+p^{\mu } r^{\nu }\right) \;\text{C}_{12}\left(p^2,-2 (p\cdot r)+p^2+r^2,r^2,m^2,m^2,m^2\right)$$

$$i \pi ^2 g^{\mu \nu } l\left(\text{C}_{00}\left(p^2,r^2,-2 (p\cdot r)+p^2+r^2,m^2,m^2,m^2\right)\right)+i \pi ^2 p^{\mu } p^{\nu } l\left(\text{C}_{11}\left(p^2,-2 (p\cdot r)+p^2+r^2,r^2,m^2,m^2,m^2\right)\right)+i \pi ^2 r^{\mu } r^{\nu } l\left(\text{C}_{11}\left(r^2,-2 (p\cdot r)+p^2+r^2,p^2,m^2,m^2,m^2\right)\right)+i \pi ^2 \left(p^{\nu } r^{\mu }+p^{\mu } r^{\nu }\right) l\left(\text{C}_{12}\left(p^2,-2 (p\cdot r)+p^2+r^2,r^2,m^2,m^2,m^2\right)\right)$$

$$\left\{l\left(\text{C}_{00}\left(p^2,r^2,-2 (p\cdot r)+p^2+r^2,m^2,m^2,m^2\right)\right),l\left(\text{C}_{11}\left(p^2,-2 (p\cdot r)+p^2+r^2,r^2,m^2,m^2,m^2\right)\right),l\left(\text{C}_{11}\left(r^2,-2 (p\cdot r)+p^2+r^2,p^2,m^2,m^2,m^2\right)\right),l\left(\text{C}_{12}\left(p^2,-2 (p\cdot r)+p^2+r^2,r^2,m^2,m^2,m^2\right)\right)\right\}$$

```mathematica
SPD[q, q]^2 FAD[{q, m}] + SPD[q, q]
FCLoopIsolate[%, {q}, DropScaleless -> True]
```

$$\frac{q^4}{q^2-m^2}+q^2$$

$$\text{FCGV}(\text{LoopInt})\left(\frac{q^4}{q^2-m^2}\right)$$

```mathematica
a FAD[{q1, m}, {q2, m}] + b FAD[{q1, m, 2}]
FCLoopIsolate[%, {q1, q2}]
FCLoopIsolate[%%, {q1, q2}, MultiLoop -> True]
```

$$\frac{a}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right)}+\frac{b}{\left(\text{q1}^2-m^2\right)^2}$$

$$a \;\text{FCGV}(\text{LoopInt})\left(\frac{1}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right)}\right)+b \;\text{FCGV}(\text{LoopInt})\left(\frac{1}{\left(\text{q1}^2-m^2\right)^2}\right)$$

$$a \;\text{FCGV}(\text{LoopInt})\left(\frac{1}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right)}\right)+\frac{b}{\left(\text{q1}^2-m^2\right)^2}$$
