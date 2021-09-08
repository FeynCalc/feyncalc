## GordonSimplify

`GordonSimplify[exp]` rewrites spinor chains describing a vector or an axial-vector current using Gordon identities.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md), [SpinorChainTrick](SpinorChainTrick.md).

### Examples

```mathematica
SpinorUBar[p1, m1] . GA[\[Mu]] . SpinorU[p2, m2]
GordonSimplify[%]
```

$$\bar{u}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.u(\text{p2},\text{m2})$$

$$\frac{\left(\overline{\text{p1}}+\overline{\text{p2}}\right)^{\mu } \left(\varphi (\overline{\text{p1}},\text{m1})\right).\left(\varphi (\overline{\text{p2}},\text{m2})\right)}{\text{m1}+\text{m2}}+\frac{i \left(\varphi (\overline{\text{p1}},\text{m1})\right).\sigma ^{\mu \overline{\text{p1}}-\overline{\text{p2}}}.\left(\varphi (\overline{\text{p2}},\text{m2})\right)}{\text{m1}+\text{m2}}$$

```mathematica
SpinorUBar[p1, m1] . GA[\[Mu], 5] . SpinorV[p2, m2]
GordonSimplify[%]
```

$$\bar{u}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.\bar{\gamma }^5.v(\text{p2},\text{m2})$$

$$\frac{\left(\overline{\text{p1}}+\overline{\text{p2}}\right)^{\mu } \left(\varphi (\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^5.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}+\text{m2}}+\frac{i \left(\varphi (\overline{\text{p1}},\text{m1})\right).\sigma ^{\mu \overline{\text{p1}}-\overline{\text{p2}}}.\bar{\gamma }^5.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}+\text{m2}}$$

Relations involving projectors can be used to trade the right projector for a left one

```mathematica
SpinorVBar[p1, m1] . GA[\[Mu], 6] . SpinorV[p2, m2]
GordonSimplify[%]
```

$$\bar{v}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.\bar{\gamma }^6.v(\text{p2},\text{m2})$$

$$-\frac{i \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\sigma ^{\mu \overline{\text{p1}}-\overline{\text{p2}}}.\bar{\gamma }^6.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}-\frac{\text{m2} \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\mu }.\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}-\frac{\left(\overline{\text{p1}}+\overline{\text{p2}}\right)^{\mu } \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^6.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}$$

Use the `Select` option to achieve the opposite

```mathematica
ex = SpinorVBar[p1, m1] . GA[\[Mu], 7] . SpinorV[p2, m2]
GordonSimplify[ex]
```

$$\bar{v}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.\bar{\gamma }^7.v(\text{p2},\text{m2})$$

$$\left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\mu }.\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)$$

```mathematica
GordonSimplify[ex, Select -> {{Spinor[__], DiracGamma[__], GA[7], Spinor[__]}}]
```

$$-\frac{i \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\sigma ^{\mu \overline{\text{p1}}-\overline{\text{p2}}}.\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}-\frac{\text{m2} \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\mu }.\bar{\gamma }^6.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}-\frac{\left(\overline{\text{p1}}+\overline{\text{p2}}\right)^{\mu } \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}$$

We can choose between having expressions proportional to $1/m_1$ (mass of the first spinor) or $1/m_2$ (mass of the second spinor) 

```mathematica
GordonSimplify[SpinorVBar[p1, m1] . GA[\[Mu], 6] . SpinorV[p2, m2], Inverse -> First]
```

$$-\frac{i \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\sigma ^{\mu \overline{\text{p1}}-\overline{\text{p2}}}.\bar{\gamma }^6.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}-\frac{\text{m2} \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\mu }.\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}-\frac{\left(\overline{\text{p1}}+\overline{\text{p2}}\right)^{\mu } \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^6.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m1}}$$

```mathematica
GordonSimplify[SpinorVBar[p1, m1] . GA[\[Mu], 6] . SpinorV[p2, m2], Inverse -> Last]
```

$$-\frac{i \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\sigma ^{\mu \overline{\text{p1}}-\overline{\text{p2}}}.\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m2}}-\frac{\text{m1} \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\mu }.\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m2}}-\frac{\left(\overline{\text{p1}}+\overline{\text{p2}}\right)^{\mu } \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)}{\text{m2}}$$

In $D$-dimensions chiral Gordon identities are scheme dependent!

```mathematica
ex = SpinorVBarD[p1, m1] . GAD[\[Mu], 5] . SpinorVD[p2, m2]
```

$$\bar{v}(\text{p1},\text{m1}).\gamma ^{\mu }.\bar{\gamma }^5.v(\text{p2},\text{m2})$$

```mathematica
FCGetDiracGammaScheme[]
GordonSimplify[ex]
```

$$\text{NDR}$$

$$-\frac{(\text{p1}+\text{p2})^{\mu } (\varphi (-\text{p1},\text{m1})).\bar{\gamma }^5.(\varphi (-\text{p2},\text{m2}))}{\text{m1}-\text{m2}}-\frac{i (\varphi (-\text{p1},\text{m1})).\sigma ^{\mu \;\text{p1}-\text{p2}}.\bar{\gamma }^5.(\varphi (-\text{p2},\text{m2}))}{\text{m1}-\text{m2}}$$

```mathematica
FCSetDiracGammaScheme["BMHV"]
GordonSimplify[ex]
```

$$\text{BMHV}$$

$$-\frac{i (\varphi (-\text{p1},\text{m1})).\sigma ^{\mu \;\text{p1}-\text{p2}}.\bar{\gamma }^5.(\varphi (-\text{p2},\text{m2}))}{\text{m1}-\text{m2}}-\frac{(\text{p1}+\text{p2})^{\mu } (\varphi (-\text{p1},\text{m1})).\bar{\gamma }^5.(\varphi (-\text{p2},\text{m2}))}{\text{m1}-\text{m2}}+\frac{2 (\varphi (-\text{p1},\text{m1})).\gamma ^{\mu }.\left(\hat{\gamma }\cdot \hat{\text{p2}}\right).\bar{\gamma }^5.(\varphi (-\text{p2},\text{m2}))}{\text{m1}-\text{m2}}$$

```mathematica
FCSetDiracGammaScheme["NDR"]
```

$$\text{NDR}$$
