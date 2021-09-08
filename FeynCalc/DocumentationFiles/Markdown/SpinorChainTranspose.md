## SpinorChainTranspose

`SpinorChainTranspose[exp]` transposes particular spinor chains in exp, which effectively switches the $u$ and $v$ spinors and reverses the order of the Dirac matrices using charge conjugation operator. This operation is often required in calculations that involve Majorana particles. By default, the function will tranpose all chains of the form $\bar{v}.x.u$ and $\bar{v}.x.v$. A different or more fine grained choice can be obtained via the option `Select`.

### See also

[Overview](Extra/FeynCalc.md), [FCChargeConjugateTransposed](FCChargeConjugateTransposed.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md).

### Examples

```mathematica
SpinorVBarD[p1, m1] . GAD[\[Mu]] . (GSD[p] + m) . GAD[\[Mu]] . SpinorUD[p2, m2]
SpinorChainTranspose[%]
```

$$\bar{v}(\text{p1},\text{m1}).\gamma ^{\mu }.(m+\gamma \cdot p).\gamma ^{\mu }.u(\text{p2},\text{m2})$$

$$-(\varphi (-\text{p2},\text{m2})).\gamma ^{\mu }.(m-\gamma \cdot p).\gamma ^{\mu }.(\varphi (\text{p1},\text{m1}))$$

```mathematica
SpinorUBarD[p1, m1] . GAD[\[Mu]] . (GSD[p] + m) . GAD[\[Mu]] . SpinorVD[p2, m2]
SpinorChainTranspose[%]
```

$$\bar{u}(\text{p1},\text{m1}).\gamma ^{\mu }.(m+\gamma \cdot p).\gamma ^{\mu }.v(\text{p2},\text{m2})$$

$$(\varphi (\text{p1},\text{m1})).\gamma ^{\mu }.(m+\gamma \cdot p).\gamma ^{\mu }.(\varphi (-\text{p2},\text{m2}))$$

```mathematica
SpinorUBarD[p1, m1] . GAD[\[Mu]] . (GSD[p] + m) . GAD[\[Mu]] . SpinorVD[p2, m2]
SpinorChainTranspose[%, Select -> {{SpinorUBarD[_, _], SpinorVD[_, _]}}]
```

$$\bar{u}(\text{p1},\text{m1}).\gamma ^{\mu }.(m+\gamma \cdot p).\gamma ^{\mu }.v(\text{p2},\text{m2})$$

$$-(\varphi (\text{p2},\text{m2})).\gamma ^{\mu }.(m-\gamma \cdot p).\gamma ^{\mu }.(\varphi (-\text{p1},\text{m1}))$$
