## Anti5

`Anti5[exp]` anticommutes all $\gamma^5$ in exp to the right. `Anti5[exp, n]` anticommutes all $\gamma^5$ $n$-times to the right. `Anti5[exp, -n]` anticommutes all $\gamma^5$ $n$-times to the left.

### See also

[Overview](Extra/FeynCalc.md), [DiracOrder](DiracOrder.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).

### Examples

```mathematica
GA[5, \[Mu]] 
Anti5[%]
Anti5[%, -1]
```

$$\bar{\gamma }^5.\bar{\gamma }^{\mu }$$

$$-\bar{\gamma }^{\mu }.\bar{\gamma }^5$$

$$\bar{\gamma }^5.\bar{\gamma }^{\mu }$$

```mathematica
GA[5, \[Alpha], \[Beta], \[Gamma], \[Delta]]
Anti5[%, 2]
Anti5[%%, Infinity]
Anti5[%, -Infinity]
```

$$\bar{\gamma }^5.\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\gamma }.\bar{\gamma }^{\delta }$$

$$\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^5.\bar{\gamma }^{\gamma }.\bar{\gamma }^{\delta }$$

$$\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\gamma }.\bar{\gamma }^{\delta }.\bar{\gamma }^5$$

$$\bar{\gamma }^5.\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\gamma }.\bar{\gamma }^{\delta }$$

In the naive $\gamma^5$-scheme $D$-dimensional $\gamma$-matrices anticommute with $\gamma^5$.

```mathematica
GA5 . GAD[\[Mu]]
Anti5[%]
```

$$\bar{\gamma }^5.\gamma ^{\mu }$$

$$-\gamma ^{\mu }.\bar{\gamma }^5$$

`Anti5` also works in the t'Hooft-Veltman-Breitenlohner-Maison scheme

```mathematica
FCSetDiracGammaScheme["BMHV"];
Anti5[GA5 . GAD[\[Mu]]]
```

$$2 \hat{\gamma }^{\mu }.\bar{\gamma }^5-\gamma ^{\mu }.\bar{\gamma }^5$$

```mathematica
FCSetDiracGammaScheme["NDR"];
```
