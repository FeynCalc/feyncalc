## DiracSigma

`DiracSigma[a, b]` stands for $I/2(a.b-b.a)$ in 4 dimensions.

`a` and `b` must have head `DiracGamma`, `GA` or `GS`. Only antisymmetry is implemented.

### See also

[Overview](Extra/FeynCalc.md), [DiracSigmaExplicit](DiracSigmaExplicit.md).

### Examples

```mathematica
DiracSigma[GA[\[Alpha]], GA[\[Beta]]]
DiracSigmaExplicit[%]
```

$$\sigma ^{\alpha \beta }$$

$$\frac{1}{2} i \left(\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }-\bar{\gamma }^{\beta }.\bar{\gamma }^{\alpha }\right)$$

```mathematica
DiracSigma[GA[\[Beta]], GA[\[Alpha]]]
```

$$-\sigma ^{\alpha \beta }$$

```mathematica
DiracSigma[GS[p], GS[q]]
DiracSigmaExplicit[%]
```

$$\sigma ^{pq}$$

$$\frac{1}{2} i \left(\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)-\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}\right)\right)$$

The antisymmetry property is built-in

```mathematica
DiracSigma[GA[\[Alpha]], GA[\[Alpha]]]
```

$$0$$
