## DiracSigmaExplicit

`DiracSigmaExplicit[exp]` inserts in exp for all `DiracSigma` its definition. `DiracSigmaExplict` is also an option of `DiracSimplify`. `DiracSigmaExplict` is also an option of various FeynCalc functions that handle the Dirac algebra.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [DiracSigma](DiracSigma.md).

### Examples

```mathematica
DiracSigma[GA[\[Alpha]], GA[\[Beta]]]
DiracSigmaExplicit[%]
```

$$\sigma ^{\alpha \beta }$$

$$\frac{1}{2} i \left(\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }-\bar{\gamma }^{\beta }.\bar{\gamma }^{\alpha }\right)$$

```mathematica
GSD[p] . DiracSigma[GAD[\[Mu]], GAD[\[Nu]]] . GSD[q]
DiracSigmaExplicit[%]
```

$$(\gamma \cdot p).\sigma ^{\mu \nu }.(\gamma \cdot q)$$

$$\frac{1}{2} i \left((\gamma \cdot p).\gamma ^{\mu }.\gamma ^{\nu }.(\gamma \cdot q)-(\gamma \cdot p).\gamma ^{\nu }.\gamma ^{\mu }.(\gamma \cdot q)\right)$$
