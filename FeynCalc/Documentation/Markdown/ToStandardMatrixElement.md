## ToStandardMatrixElement

`ToStandardMatrixElement[exp]` wraps Dirac structures, color structures and polarization vectors with the head `StandardMatrixElement`.

The idea of having standard matrix elements stems from A. Denner's "Techniques for the calculation of electroweak radiative corrections at the one-loop level and results for W-physics at LEP200", cf. [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).

### See also

[Overview](Extra/FeynCalc.md), [DiracSubstitute5](DiracSubstitute5.md), [DiracGamma](DiracGamma.md), [ToDiracGamma67](ToDiracGamma67.md), [Spinor](Spinor.md).

### Examples

```mathematica
Spinor[Momentum[k2, D], 0, 1] . GAD[\[Mu]] . Spinor[-Momentum[k1, D], 0, 1]*Spinor[-Momentum[ps, D], SMP["m_s"], 1] . GAD[\[Mu]] . Spinor[Momentum[pd, D], SMP["m_d"], 1]
ToStandardMatrixElement[%]
```

$$(\varphi (\text{k2})).\gamma ^{\mu }.(\varphi (-\text{k1})) \left(\varphi (-\text{ps},m_s)\right).\gamma ^{\mu }.\left(\varphi (\text{pd},m_d)\right)$$

$$\left\| (\varphi (\text{k2})).\gamma ^{\mu }.\bar{\gamma }^6.(\varphi (-\text{k1})) \left(\varphi (-\text{ps},m_s)\right).\gamma ^{\mu }.\bar{\gamma }^6.\left(\varphi (\text{pd},m_d)\right)\right\| +\left\| (\varphi (\text{k2})).\gamma ^{\mu }.\bar{\gamma }^7.(\varphi (-\text{k1})) \left(\varphi (-\text{ps},m_s)\right).\gamma ^{\mu }.\bar{\gamma }^6.\left(\varphi (\text{pd},m_d)\right)\right\| +\left\| (\varphi (\text{k2})).\gamma ^{\mu }.\bar{\gamma }^6.(\varphi (-\text{k1})) \left(\varphi (-\text{ps},m_s)\right).\gamma ^{\mu }.\bar{\gamma }^7.\left(\varphi (\text{pd},m_d)\right)\right\| +\left\| (\varphi (\text{k2})).\gamma ^{\mu }.\bar{\gamma }^7.(\varphi (-\text{k1})) \left(\varphi (-\text{ps},m_s)\right).\gamma ^{\mu }.\bar{\gamma }^7.\left(\varphi (\text{pd},m_d)\right)\right\|$$