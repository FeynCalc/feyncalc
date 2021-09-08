## FCTraceExpand

`FCTraceExpand[exp]` expands traces of Dirac and $SU(N)$ matrices using linearity of the trace. The traces themselves are not evaluated.

### See also

[Overview](Extra/FeynCalc.md), [DiracTrace](DiracTrace.md), [SUNTrace](SUNTrace.md).

### Examples

```mathematica
ex = DiracTrace[GA[\[Mu]] . (GS[p1] + m1) . GA[\[Nu]] . (GS[p2] + m2) . GA[\[Rho]] +x]
```

$$\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\rho }+x\right)$$

```mathematica
FCTraceExpand[ex]
```

$$\text{m1} \;\text{m2} \;\text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }\right)+\text{m1} \;\text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}\right).\bar{\gamma }^{\rho }\right)+\text{m2} \;\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}\right).\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }\right)+\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}\right).\bar{\gamma }^{\rho }\right)+\text{tr}(1) x$$

```mathematica
FCTraceExpand[ex, DotSimplify -> False]
```

$$\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\rho }\right)+\text{tr}(1) x$$

```mathematica
FCTraceExpand[ex, DiracTrace -> False]
```

$$\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\rho }+x\right)$$

```mathematica
a*DiracTrace[GA[\[Mu]] . (GS[p1] + m1) . GA[\[Nu]]] + b*DiracTrace[GA[\[Mu]] . (GS[p2] + m2) . GA[\[Nu]]]
FCTraceExpand[%, Momentum -> {p1}]
```

$$a \;\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}+\text{m1}\right).\bar{\gamma }^{\nu }\right)+b \;\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\nu }\right)$$

$$a \left(\text{m1} \;\text{tr}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)+\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p1}}\right).\bar{\gamma }^{\nu }\right)\right)+b \;\text{tr}\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{\text{p2}}+\text{m2}\right).\bar{\gamma }^{\nu }\right)$$

At the moment `SUNTrace` automatically expands its content, so here `FCTraceExpand` is not needed. However, this may change in future.

```mathematica
ex = SUNTrace[SUNT[i, j, k] + SUNT[l, m, n]]
```

$$\text{tr}(T^i.T^j.T^k)+\text{tr}(T^l.T^m.T^n)$$

```mathematica
FCTraceExpand[ex]
```

$$\text{tr}(T^i.T^j.T^k)+\text{tr}(T^l.T^m.T^n)$$

```mathematica
FCTraceExpand[ex, SUNTrace -> False]
```

$$\text{tr}(T^i.T^j.T^k)+\text{tr}(T^l.T^m.T^n)$$
