## DiracSubstitute5

DiracSubstitute5[exp] rewrites $\gamma^5$ in terms of the chirality projectors $\gamma^6$ and $\gamma^7$. `DiracSubstitute5` is also an option of various FeynCalc functions that handle Dirac algebra.

### See also

[Overview](Extra/FeynCalc.md), [DiracSubstitute67](DiracSubstitute67.md), [DiracGamma](DiracGamma.md), [ToDiracGamma67](ToDiracGamma67.md).

### Examples

```mathematica
GA[5]
DiracSubstitute5[%]
```

$$\bar{\gamma }^5$$

$$\bar{\gamma }^6-\bar{\gamma }^7$$

```mathematica
SpinorUBar[Subscript[p, 1]] . GA[\[Mu]] . GA[5] . GA[\[Nu]] . SpinorU[Subscript[p, 2]]
DiracSubstitute5[%]
```

$$\bar{u}\left(p_1\right).\bar{\gamma }^{\mu }.\bar{\gamma }^5.\bar{\gamma }^{\nu }.u\left(p_2\right)$$

$$\left(\varphi (\overline{p}_1)\right).\bar{\gamma }^{\mu }.\left(\bar{\gamma }^6-\bar{\gamma }^7\right).\bar{\gamma }^{\nu }.\left(\varphi (\overline{p}_2)\right)$$
