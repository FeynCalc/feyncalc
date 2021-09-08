## DiracReduce

`DiracReduce[exp]` reduces all $4$-dimensional Dirac matrices in exp to the standard basis $(S,P,V,A,T)$ using the Chisholm identity.

In the result the basic Dirac structures can be wrapped with a head `DiracBasis`, that is

- $S$: `DiracBasis[1]`
- $P$: `DiracBasis[GA[5]]`
- $V$: `DiracBasis[GA[$\mu$]]`
- $A$: `DiracBasis[GA[$\mu$,5]]`
- $T$: `DiracBasis[DiracSigma[GA[$\mu$,$\nu$]]]`

By default `DiracBasis` is substituted to `Identity`.

### See also

[Overview](Extra/FeynCalc.md), [Chisholm](Chisholm.md), [DiracSimplify](DiracSimplify.md), [EpsChisholm](EpsChisholm.md).

### Examples

```mathematica
GA[\[Mu], \[Nu]]
DiracReduce[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$\bar{g}^{\mu \nu }-i \sigma ^{\mu \nu }$$

`DiracReduce` only works with Dirac matrices in $4$ dimensions, $D$-dimensional matrices are ignored.

```mathematica
GAD[\[Mu], \[Nu]]
DiracReduce[%]
```

$$\gamma ^{\mu }.\gamma ^{\nu }$$

$$\gamma ^{\mu }.\gamma ^{\nu }$$

```mathematica
SpinorUBar[Subscript[p, 1], Subscript[m, 1]] . GA[\[Mu], \[Nu], \[Rho]] . SpinorV[Subscript[p, 2], Subscript[m, 2]]
DiracReduce[%]
```

$$\bar{u}\left(p_1,m_1\right).\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.v\left(p_2,m_2\right)$$

$$i \bar{\epsilon }^{\mu \nu \rho \;\text{\$MU}(\text{\$36})} \left(\varphi (\overline{p}_1,m_1)\right).\bar{\gamma }^{\text{\$MU}(\text{\$36})}.\bar{\gamma }^5.\left(\varphi (-\overline{p}_2,m_2)\right)+\bar{g}^{\mu \nu } \left(\varphi (\overline{p}_1,m_1)\right).\bar{\gamma }^{\rho }.\left(\varphi (-\overline{p}_2,m_2)\right)-\bar{g}^{\mu \rho } \left(\varphi (\overline{p}_1,m_1)\right).\bar{\gamma }^{\nu }.\left(\varphi (-\overline{p}_2,m_2)\right)+\bar{g}^{\nu \rho } \left(\varphi (\overline{p}_1,m_1)\right).\bar{\gamma }^{\mu }.\left(\varphi (-\overline{p}_2,m_2)\right)$$

```mathematica
GA[\[Mu], \[Nu], \[Rho], \[Sigma]]
DiracReduce[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }$$

$$-i \bar{\gamma }^5 \bar{\epsilon }^{\mu \nu \rho \sigma }-i \sigma ^{\rho \sigma } \bar{g}^{\mu \nu }+i \sigma ^{\nu \sigma } \bar{g}^{\mu \rho }-i \sigma ^{\nu \rho } \bar{g}^{\mu \sigma }-i \sigma ^{\mu \sigma } \bar{g}^{\nu \rho }+i \sigma ^{\mu \rho } \bar{g}^{\nu \sigma }-i \sigma ^{\mu \nu } \bar{g}^{\rho \sigma }+\bar{g}^{\mu \sigma } \bar{g}^{\nu \rho }-\bar{g}^{\mu \rho } \bar{g}^{\nu \sigma }+\bar{g}^{\mu \nu } \bar{g}^{\rho \sigma }$$

Do some checks of the results

```mathematica
DiracSimplify[GA[\[Mu], \[Nu], \[Rho], \[Sigma]] . GA[\[Mu], \[Nu], \[Rho], \[Sigma]]]
```

$$-128$$

```mathematica
DiracSimplify[DiracReduce[GA[\[Mu], \[Nu], \[Rho], \[Sigma]]] . DiracReduce[GA[\[Mu], \[Nu], \[Rho], \[Sigma]]]]
```

$$-128$$

We may also keep the head `DiracBasis` in the final result

```mathematica
DiracReduce[GA[\[Mu], \[Nu], \[Rho], \[Sigma]], FinalSubstitutions -> {}]
```

$$-i \bar{g}^{\mu \nu } \;\text{DiracBasis}\left(\text{DiracSigma}\left(\text{DiracBasis}\left(\bar{\gamma }^{\rho }\right),\text{DiracBasis}\left(\bar{\gamma }^{\sigma }\right)\right)\right)+i \bar{g}^{\mu \rho } \;\text{DiracBasis}\left(\text{DiracSigma}\left(\text{DiracBasis}\left(\bar{\gamma }^{\nu }\right),\text{DiracBasis}\left(\bar{\gamma }^{\sigma }\right)\right)\right)-i \bar{g}^{\mu \sigma } \;\text{DiracBasis}\left(\text{DiracSigma}\left(\text{DiracBasis}\left(\bar{\gamma }^{\nu }\right),\text{DiracBasis}\left(\bar{\gamma }^{\rho }\right)\right)\right)-i \bar{g}^{\nu \rho } \;\text{DiracBasis}\left(\text{DiracSigma}\left(\text{DiracBasis}\left(\bar{\gamma }^{\mu }\right),\text{DiracBasis}\left(\bar{\gamma }^{\sigma }\right)\right)\right)+i \bar{g}^{\nu \sigma } \;\text{DiracBasis}\left(\text{DiracSigma}\left(\text{DiracBasis}\left(\bar{\gamma }^{\mu }\right),\text{DiracBasis}\left(\bar{\gamma }^{\rho }\right)\right)\right)-i \bar{g}^{\rho \sigma } \;\text{DiracBasis}\left(\text{DiracSigma}\left(\text{DiracBasis}\left(\bar{\gamma }^{\mu }\right),\text{DiracBasis}\left(\bar{\gamma }^{\nu }\right)\right)\right)-i \;\text{DiracBasis}\left(\bar{\gamma }^5\right) \bar{\epsilon }^{\mu \nu \rho \sigma }+\text{DiracBasis}(1) \bar{g}^{\mu \sigma } \bar{g}^{\nu \rho }-\text{DiracBasis}(1) \bar{g}^{\mu \rho } \bar{g}^{\nu \sigma }+\text{DiracBasis}(1) \bar{g}^{\mu \nu } \bar{g}^{\rho \sigma }$$