## FCLoopExtract

`FCLoopExtract[expr, {q1, q2, ...}, loopHead]` exctracts loop integrals that depend on `q1, q2, ...` from the given expression. The output is given as a list of three entries. The first one contains part of the original expression that consists of irrelevant loop integrals and terms that are free of any loop integrals. The second entry contains relevant loop integrals, where each integral is wrapped into `loopHead`. The third entry is a list of all the unique loop integrals from the second entry and can be used as an input to another function. Note that if loop integrals contain free indices, those will not be canonicalized.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopIsolate](FCLoopIsolate.md).

### Examples

```mathematica
FCI[GSD[q - p1] . (GSD[q - p2] + M) . GSD[p3] SPD[q, p2] FAD[q, q - p1, {q - p2, m}]]
FCLoopExtract[%, {q}, loopInt]
```

$$\frac{(\text{p2}\cdot q) (\gamma \cdot (q-\text{p1})).(M+\gamma \cdot (q-\text{p2})).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}$$

$$\left\{0,((\gamma \cdot \;\text{p1}).(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p3})-M (\gamma \cdot \;\text{p1}).(\gamma \cdot \;\text{p3})) \;\text{loopInt}\left(\frac{\text{p2}\cdot q}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)+M \;\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)-\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot \;\text{p1}).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)-\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)+\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\left\{\text{loopInt}\left(\frac{\text{p2}\cdot q}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot \;\text{p1}).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right),\text{loopInt}\left(\frac{(\text{p2}\cdot q) (\gamma \cdot q).(\gamma \cdot q).(\gamma \cdot \;\text{p3})}{q^2.(q-\text{p1})^2.\left((q-\text{p2})^2-m^2\right)}\right)\right\}\right\}$$
