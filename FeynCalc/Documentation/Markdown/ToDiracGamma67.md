## ToDiracGamma67

`ToDiracGamma67[exp]` substitutes $\frac{1}{2} \left(1 + \gamma^5\right)$ and $\frac{1}{2}\left(1-\gamma^5\right)$ by the chirality projectors $\gamma^6$ and $\gamma^7$.

### See also

[Overview](Extra/FeynCalc.md), [DiracSubstitute5](DiracSubstitute5.md), [DiracGamma](DiracGamma.md), [ToDiracGamma67](ToDiracGamma67.md).

### Examples

```mathematica
GA[\[Mu]] . (1/2 + GA[5]/2) . GA[\[Nu]]
ToDiracGamma67[%]
```

$$\bar{\gamma }^{\mu }.\left(\frac{\bar{\gamma }^5}{2}+\frac{1}{2}\right).\bar{\gamma }^{\nu }$$

$$\bar{\gamma }^{\mu }.\bar{\gamma }^6.\bar{\gamma }^{\nu }$$

When the option `All` is set to `True`, also standalone $\gamma^5$ will be replaced

```mathematica
GA[\[Mu], 5, \[Nu]]
ToDiracGamma67[%, All -> True]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^5.\bar{\gamma }^{\nu }$$

$$\bar{\gamma }^{\mu }.\left(\bar{\gamma }^6-\bar{\gamma }^7\right).\bar{\gamma }^{\nu }$$
