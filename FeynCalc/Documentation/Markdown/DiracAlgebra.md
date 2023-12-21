```mathematica
 
```

## Dirac algebra

### See also

[Overview](Extra/FeynCalc.md).

### Simplifications

The two most relevant functions for the manipulations of Dirac matrices are `DiracSimplify` and `DiracTrace`.

The goal of `DiracSimplify` is to eliminate all pairs of Dirac matrices with the equal indices or contracted with the same $4$-vectors

```mathematica
GA[\[Mu]] . GS[p + m] . GA[\[Mu]]
DiracSimplify[%]
```

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \left(\overline{m}+\overline{p}\right)\right).\bar{\gamma }^{\mu }$$

$$-2 \bar{\gamma }\cdot \overline{m}-2 \bar{\gamma }\cdot \overline{p}$$

```mathematica
GA[\[Mu]] . GS[p + m1] . GA[\[Nu]] . GS[p + m2]
DiracSimplify[%]
```

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \left(\overline{\text{m1}}+\overline{p}\right)\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \left(\overline{\text{m2}}+\overline{p}\right)\right)$$

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{m1}}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{m2}}\right)+\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{m1}}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{p}\right)+\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{m2}}\right)-\overline{p}^2 \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }+2 \overline{p}^{\nu } \bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}\right)$$

`DiracTrace` is used for the evaluation of Dirac traces. The trace is not evaluated by default

```mathematica
DiracTrace[GA[\[Mu], \[Nu]]]
```

$$\text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)$$

To obtain the result we can either use the option `DiracTraceEvaluate`

```mathematica
DiracTrace[GA[\[Mu], \[Nu]], DiracTraceEvaluate -> True]
```

$$4 \bar{g}^{\mu \nu }$$

or use `DiracSimplify` instead.

By default FeynCalc refuses to compute a $D$-dimensional trace that contains $\gamma^5$

```mathematica
DiracTrace[GAD[\[Alpha], \[Beta], \[Mu], \[Nu], \[Rho], \[Sigma], 5]] // DiracSimplify
```

$$\text{tr}\left(\gamma ^{\alpha }.\gamma ^{\beta }.\gamma ^{\mu }.\gamma ^{\nu }.\gamma ^{\rho }.\gamma ^{\sigma }.\bar{\gamma }^5\right)$$

This is because by default FeynCalc is using anticommuting $\gamma^5$ in $D$-dimensions, a scheme known as Naive Dimensional Regularization (NDR)

```mathematica
DiracSimplify[GAD[\[Mu]] . GA[5] . GAD[\[Nu]]]
```

$$-\gamma ^{\mu }.\gamma ^{\nu }.\bar{\gamma }^5$$

In general, a chiral trace is a very ambiguous object in NDR. The results depends on the position of $\gamma^5$ inside the trace, so that we chose not to produce results that might be potentially inconsistent. However, FeynCalc can also be told to use the Breitenlohner-Maison-t'Hooft-Veltman scheme (BMHV), which is an algebraically consistent scheme (but has other issues, e.g. it breaks Ward identities)

```mathematica
FCSetDiracGammaScheme["BMHV"];
```

Notice that now FeynCalc anticommutes $\gamma^5$ according to the BMHV algebra, which leads to the appearance of $D-4$-dimensional Dirac matrices

```mathematica
DiracSimplify[GAD[\[Mu]] . GA[5] . GAD[\[Nu]]]
```

$$2 \gamma ^{\mu }.\hat{\gamma }^{\nu }.\bar{\gamma }^5-\gamma ^{\mu }.\gamma ^{\nu }.\bar{\gamma }^5$$

Also Dirac traces are not an issue now

```mathematica
DiracTrace[GAD[\[Alpha], \[Beta], \[Mu], \[Nu], \[Rho], \[Sigma]] . GA[5]] // DiracSimplify
```

$$-4 i g^{\alpha \beta } \bar{\epsilon }^{\mu \nu \rho \sigma }+4 i g^{\alpha \mu } \bar{\epsilon }^{\beta \nu \rho \sigma }-4 i g^{\alpha \nu } \bar{\epsilon }^{\beta \mu \rho \sigma }+4 i g^{\alpha \rho } \bar{\epsilon }^{\beta \mu \nu \sigma }-4 i g^{\alpha \sigma } \bar{\epsilon }^{\beta \mu \nu \rho }-4 i g^{\beta \mu } \bar{\epsilon }^{\alpha \nu \rho \sigma }+4 i g^{\beta \nu } \bar{\epsilon }^{\alpha \mu \rho \sigma }-4 i g^{\beta \rho } \bar{\epsilon }^{\alpha \mu \nu \sigma }+4 i g^{\beta \sigma } \bar{\epsilon }^{\alpha \mu \nu \rho }-4 i g^{\mu \nu } \bar{\epsilon }^{\alpha \beta \rho \sigma }+4 i g^{\mu \rho } \bar{\epsilon }^{\alpha \beta \nu \sigma }-4 i g^{\mu \sigma } \bar{\epsilon }^{\alpha \beta \nu \rho }-4 i g^{\nu \rho } \bar{\epsilon }^{\alpha \beta \mu \sigma }+4 i g^{\nu \sigma } \bar{\epsilon }^{\alpha \beta \mu \rho }-4 i g^{\rho \sigma } \bar{\epsilon }^{\alpha \beta \mu \nu }$$

To compute chiral traces in the BMHV scheme, FeynCalc uses [West's formula](https://inspirehep.net/record/31057). Still, NDR is the default scheme in FeynCalc.

In tree-level calculation a useful operation is the so-called SPVAT-decomposition of Dirac chains.
This is done using `DiracReduce`

```mathematica
GA[\[Mu], \[Nu], \[Rho]] . GS[p] . GA[\[Alpha]]
DiracReduce[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\left(\bar{\gamma }\cdot \overline{p}\right).\bar{\gamma }^{\alpha }$$

$$-i \bar{g}^{\mu \nu } \bar{\gamma }^{\text{\$MU}(\text{\$68})}.\bar{\gamma }^5 \bar{\epsilon }^{\alpha \rho \;\text{\$MU}(\text{\$68})\overline{p}}+i \bar{g}^{\alpha \rho } \bar{\gamma }^{\text{\$MU}(\text{\$70})}.\bar{\gamma }^5 \bar{\epsilon }^{\mu \nu \;\text{\$MU}(\text{\$70})\overline{p}}+i \overline{p}^{\alpha } \bar{\gamma }^{\text{\$MU}(\text{\$71})}.\bar{\gamma }^5 \bar{\epsilon }^{\mu \nu \rho \;\text{\$MU}(\text{\$71})}+i \overline{p}^{\rho } \bar{\gamma }^{\text{\$MU}(\text{\$72})}.\bar{\gamma }^5 \bar{\epsilon }^{\alpha \mu \nu \;\text{\$MU}(\text{\$72})}+\bar{\gamma }^{\rho } \overline{p}^{\alpha } \bar{g}^{\mu \nu }-\bar{\gamma }^{\nu } \overline{p}^{\alpha } \bar{g}^{\mu \rho }-\bar{\gamma }^{\rho } \overline{p}^{\mu } \bar{g}^{\alpha \nu }+\bar{\gamma }^{\nu } \overline{p}^{\mu } \bar{g}^{\alpha \rho }+\bar{\gamma }^{\mu } \overline{p}^{\alpha } \bar{g}^{\nu \rho }+\bar{\gamma }^{\alpha } \overline{p}^{\mu } \bar{g}^{\nu \rho }+\bar{\gamma }^{\rho } \overline{p}^{\nu } \bar{g}^{\alpha \mu }-\bar{\gamma }^{\mu } \overline{p}^{\nu } \bar{g}^{\alpha \rho }-\bar{\gamma }^{\alpha } \overline{p}^{\nu } \bar{g}^{\mu \rho }-\bar{\gamma }^{\nu } \overline{p}^{\rho } \bar{g}^{\alpha \mu }+\bar{\gamma }^{\mu } \overline{p}^{\rho } \bar{g}^{\alpha \nu }+\bar{\gamma }^{\alpha } \overline{p}^{\rho } \bar{g}^{\mu \nu }-\bar{g}^{\alpha \rho } \bar{g}^{\mu \nu } \bar{\gamma }\cdot \overline{p}+\bar{g}^{\alpha \nu } \bar{g}^{\mu \rho } \bar{\gamma }\cdot \overline{p}-\bar{g}^{\alpha \mu } \bar{g}^{\nu \rho } \bar{\gamma }\cdot \overline{p}-i \bar{\gamma }^{\nu }.\bar{\gamma }^5 \bar{\epsilon }^{\alpha \mu \rho \overline{p}}+i \bar{\gamma }^{\mu }.\bar{\gamma }^5 \bar{\epsilon }^{\alpha \nu \rho \overline{p}}$$

Gordon's identities are implemented via `GordonSimplify`

```mathematica
SpinorUBar[p1, m1] . GA[\[Mu]] . SpinorU[p2, m2]
GordonSimplify[%]
```

$$\bar{u}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.u(\text{p2},\text{m2})$$

$$\frac{\left(\overline{\text{p1}}+\overline{\text{p2}}\right)^{\mu } \left(\varphi (\overline{\text{p1}},\text{m1})\right).\left(\varphi (\overline{\text{p2}},\text{m2})\right)}{\text{m1}+\text{m2}}+\frac{i \left(\varphi (\overline{\text{p1}},\text{m1})\right).\sigma ^{\mu \overline{\text{p1}}-\overline{\text{p2}}}.\left(\varphi (\overline{\text{p2}},\text{m2})\right)}{\text{m1}+\text{m2}}$$