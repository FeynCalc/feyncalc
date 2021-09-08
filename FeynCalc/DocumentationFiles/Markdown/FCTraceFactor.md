## FCTraceFactor

`FCTraceFactor[expr]` factors out all expressions inside a trace to which the trace doesn't apply. For example, all objects that are not Dirac matrices can be safely factored out from every Dirac trace.

### See also

[Overview](Extra/FeynCalc.md), [DiracTrace](DiracTrace.md), [SUNTrace](SUNTrace.md).

### Examples

Pull constants out of the Dirac trace

```mathematica
FCTraceFactor[DiracTrace[c1 . (c2*(GS[p1] + M)) . GA[\[Mu]] . (c3*(GS[p2] + M2))]]
```

$$\text{c1} \;\text{c2} \;\text{c3} \;\text{tr}\left(\left(\bar{\gamma }\cdot \overline{\text{p1}}+M\right).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{M2}\right)\right)$$
