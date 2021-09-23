## SpinorChainTrick

`SpinorChainTrick[exp]` applies several simplifications to products of spinor chains.

### See also

[Overview](Extra/FeynCalc.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md).

### Examples

```mathematica
a SpinorUBar[p1, m1] . GA[\[Mu]] . SpinorU[p2, m2] SpinorVBar[p1, m1] . GA[\[Mu]] . SpinorV[p4, m4] + b SpinorUBar[p1, m1] . GA[\[Nu]] . SpinorU[p2, m2] SpinorVBar[p1, m1] . GA[\[Nu]] . SpinorV[p4, m4]
SpinorChainTrick[%]
```

$$a \bar{u}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.u(\text{p2},\text{m2}) \bar{v}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.v(\text{p4},\text{m4})+b \bar{u}(\text{p1},\text{m1}).\bar{\gamma }^{\nu }.u(\text{p2},\text{m2}) \bar{v}(\text{p1},\text{m1}).\bar{\gamma }^{\nu }.v(\text{p4},\text{m4})$$

$$(a+b) \left(\varphi (\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\text{FCGV}(\text{LI241})}.\left(\varphi (\overline{\text{p2}},\text{m2})\right) \left(\varphi (-\overline{\text{p1}},\text{m1})\right).\bar{\gamma }^{\text{FCGV}(\text{LI241})}.\left(\varphi (-\overline{\text{p4}},\text{m4})\right)$$

```mathematica
SpinorUBar[p1, m1] . GAE[\[Mu]] . SpinorU[p2, m2] SpinorVBar[p1, m1] . GA[\[Mu]] . SpinorV[p4, m4]
SpinorChainTrick[%]
```

$$\bar{u}(\text{p1},\text{m1}).\hat{\gamma }^{\mu }.u(\text{p2},\text{m2}) \bar{v}(\text{p1},\text{m1}).\bar{\gamma }^{\mu }.v(\text{p4},\text{m4})$$

$$0$$
