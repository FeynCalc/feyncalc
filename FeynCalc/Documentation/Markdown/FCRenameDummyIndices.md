## FCRenameDummyIndices

`FCRenameDummyIndices[expr]` identifies all dummy Lorentz and $SU(N)$ indices and changes their names pairwise to random symbols. This can be useful if you have an expression that contains dummy indices and want to compute the square of it. For example, the square of `GA[a, l, a]` equals $16$. However, if you forget to rename the dummy indices and compute `GA[a, l, a, a, l, a]` instead of `GA[a, l, a, b, l, b]`, you will get $64$.

### See also

[Overview](Extra/FeynCalc.md), [ComplexConjugate](ComplexConjugate.md).

### Examples

```mathematica
FVD[q, mu] FVD[p, mu] + FVD[q, nu] FVD[p, nu] + FVD[q, si] FVD[r, si]
FCRenameDummyIndices[%] // Factor2
```

$$p^{\text{mu}} q^{\text{mu}}+p^{\text{nu}} q^{\text{nu}}+q^{\text{si}} r^{\text{si}}$$

$$p^{\text{\$AL}(\text{\$24})} q^{\text{\$AL}(\text{\$24})}+p^{\text{\$AL}(\text{\$25})} q^{\text{\$AL}(\text{\$25})}+q^{\text{\$AL}(\text{\$26})} r^{\text{\$AL}(\text{\$26})}$$

```mathematica
Uncontract[SPD[q, p]^2, q, p, Pair -> All]
FCRenameDummyIndices[%]
```

$$p^{\text{\$AL}(\text{\$27})} p^{\text{\$AL}(\text{\$28})} q^{\text{\$AL}(\text{\$27})} q^{\text{\$AL}(\text{\$28})}$$

$$p^{\text{\$AL}(\text{\$29})} p^{\text{\$AL}(\text{\$30})} q^{\text{\$AL}(\text{\$29})} q^{\text{\$AL}(\text{\$30})}$$

```mathematica
amp = -(Spinor[Momentum[k1], SMP["m_mu"], 1] . GA[Lor1] . Spinor[-Momentum[k2], SMP["m_mu"], 1]*Spinor[-Momentum[p2], SMP["m_e"], 1] . GA[Lor1] . Spinor[Momentum[p1], SMP["m_e"], 1]*FAD[k1 + k2, Dimension -> 4]*SMP["e"]^2);
amp // FCRenameDummyIndices
```

$$-\frac{\text{e}^2 \left(\varphi (-\overline{\text{p2}},m_e)\right).\bar{\gamma }^{\text{\$AL}(\text{\$31})}.\left(\varphi (\overline{\text{p1}},m_e)\right) \left(\varphi (\overline{\text{k1}},m_{\mu })\right).\bar{\gamma }^{\text{\$AL}(\text{\$31})}.\left(\varphi (-\overline{\text{k2}},m_{\mu })\right)}{(\overline{\text{k1}}+\overline{\text{k2}})^2}$$
