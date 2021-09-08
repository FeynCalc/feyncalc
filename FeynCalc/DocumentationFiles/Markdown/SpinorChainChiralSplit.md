## SpinorChainChiralSplit

`SpinorChainChiralSplit[exp]` introduces chiral projectors in spinor chains that contain no $\gamma^5$.

### See also

[Overview](Extra/FeynCalc.md), [DiracSubstitute67](DiracSubstitute67.md), [DiracGamma](DiracGamma.md), [ToDiracGamma67](ToDiracGamma67.md).

### Examples

```mathematica
SpinorUBar[p1, m1] . GSD[p] . SpinorV[p2, m2]
SpinorChainChiralSplit[%]
```

$$\bar{u}(\text{p1},\text{m1}).(\gamma \cdot p).v(\text{p2},\text{m2})$$

$$\left(\varphi (\overline{\text{p1}},\text{m1})\right).(\gamma \cdot p).\bar{\gamma }^6.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)+\left(\varphi (\overline{\text{p1}},\text{m1})\right).(\gamma \cdot p).\bar{\gamma }^7.\left(\varphi (-\overline{\text{p2}},\text{m2})\right)$$
