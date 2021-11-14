## ComplexConjugate

`ComplexConjugate[exp]` returns the complex conjugate of `exp`, where the input expression must be a proper matrix element. All Dirac matrices are assumed to be inside closed Dirac spinor chains. If this is not the case, the result will be inconsistent. Denominators may not contain explicit $i$'s.

### See also

[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [FermionSpinSum](FermionSpinSum.md), [DiracGamma](DiracGamma.md).

### Examples

ComplexConjugate is meant to be applied to amplitudes, i.e. given a matrix element $\mathcal{M}$, it will return $\mathcal{M}^\ast$.

```mathematica
amp = (Spinor[Momentum[k1], SMP["m_e"], 1] . GA[\[Mu]] . Spinor[Momentum[p2], SMP["m_e"], 1]*
     Spinor[Momentum[k2], SMP["m_e"], 1] . GA[\[Nu]] . Spinor[Momentum[p1], SMP["m_e"], 1]*
     FAD[k1 - p2, Dimension -> 4]*SMP["e"]^2 - Spinor[Momentum[k1], SMP["m_e"], 
       1] . GA[\[Mu]] . Spinor[Momentum[p1], SMP["m_e"], 1]*Spinor[Momentum[k2], 
       SMP["m_e"], 1] . GA[\[Nu]] . Spinor[Momentum[p2], SMP["m_e"], 1]*FAD[k2 - p2, 
      Dimension -> 4]*SMP["e"]^2)
```

$$\frac{\text{e}^2 \left(\varphi (\overline{\text{k1}},m_e)\right).\bar{\gamma }^{\mu }.\left(\varphi (\overline{\text{p2}},m_e)\right) \left(\varphi (\overline{\text{k2}},m_e)\right).\bar{\gamma }^{\nu }.\left(\varphi (\overline{\text{p1}},m_e)\right)}{(\overline{\text{k1}}-\overline{\text{p2}})^2}-\frac{\text{e}^2 \left(\varphi (\overline{\text{k1}},m_e)\right).\bar{\gamma }^{\mu }.\left(\varphi (\overline{\text{p1}},m_e)\right) \left(\varphi (\overline{\text{k2}},m_e)\right).\bar{\gamma }^{\nu }.\left(\varphi (\overline{\text{p2}},m_e)\right)}{(\overline{\text{k2}}-\overline{\text{p2}})^2}$$

```mathematica
ComplexConjugate[amp]
```

$$\frac{\text{e}^2 \left(\varphi (\overline{\text{p2}},m_e)\right).\bar{\gamma }^{\mu }.\left(\varphi (\overline{\text{k1}},m_e)\right) \left(\varphi (\overline{\text{p1}},m_e)\right).\bar{\gamma }^{\nu }.\left(\varphi (\overline{\text{k2}},m_e)\right)}{(\overline{\text{k1}}-\overline{\text{p2}})^2}-\frac{\text{e}^2 \left(\varphi (\overline{\text{p1}},m_e)\right).\bar{\gamma }^{\mu }.\left(\varphi (\overline{\text{k1}},m_e)\right) \left(\varphi (\overline{\text{p2}},m_e)\right).\bar{\gamma }^{\nu }.\left(\varphi (\overline{\text{k2}},m_e)\right)}{(\overline{\text{k2}}-\overline{\text{p2}})^2}$$

Although one can also apply the function to standalone Dirac matrices, it should be understood that the result is not equivalent to the complex conjugation of such matrices.

```mathematica
GA[\[Mu]]
ComplexConjugate[%]
```

$$\bar{\gamma }^{\mu }$$

$$\bar{\gamma }^{\mu }$$

```mathematica
GA[5]
ComplexConjugate[%]
```

$$\bar{\gamma }^5$$

$$-\bar{\gamma }^5$$

```mathematica
(GS[Polarization[k1, -I, Transversality -> True]] . (GS[k1 - p2] + SMP["m_e"]) . 
   GS[Polarization[k2, -I, Transversality -> True]])
ComplexConjugate[%]
```

$$\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k1})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)+m_e\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k2})\right)$$

$$\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{k2})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}-\overline{\text{p2}}\right)+m_e\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{k1})\right)$$

```mathematica
SUNTrace[SUNT[a, b, c]]
ComplexConjugate[%]
```

$$\text{tr}(T^a.T^b.T^c)$$

$$\text{tr}(T^c.T^b.T^a)$$

Since FeynCalc 9.3 `ComplexConjugate` will automatically rename dummy indices.

```mathematica
PolarizationVector[p1, \[Mu]] PolarizationVector[p2, \[Nu]] MT[\[Mu], \[Nu]]
ComplexConjugate[%]
```

$$\bar{g}^{\mu \nu } \bar{\varepsilon }^{\mu }(\text{p1}) \bar{\varepsilon }^{\nu }(\text{p2})$$

$$\bar{g}^{\text{\$AL}(\text{\$19})\text{\$AL}(\text{\$20})} \bar{\varepsilon }^{*\text{\$AL}(\text{\$19})}(\text{p1}) \bar{\varepsilon }^{*\text{\$AL}(\text{\$20})}(\text{p2})$$

```mathematica
GA[\[Mu], \[Nu]] LC[\[Mu], \[Nu]][p1, p2]
ComplexConjugate[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu } \bar{\epsilon }^{\mu \nu \overline{\text{p1}}\;\overline{\text{p2}}}$$

$$\bar{\gamma }^{\text{\$AL}(\text{\$21})}.\bar{\gamma }^{\text{\$AL}(\text{\$22})} \bar{\epsilon }^{\text{\$AL}(\text{\$22})\text{\$AL}(\text{\$21})\overline{\text{p1}}\;\overline{\text{p2}}}$$

This behavior can be disabled by setting the option `FCRenameDummyIndices` to `False`.

```mathematica
ComplexConjugate[GA[\[Mu], \[Nu]] LC[\[Mu], \[Nu]][p1, p2], FCRenameDummyIndices -> False]
```

$$\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu } \bar{\epsilon }^{\mu \nu \overline{\text{p1}}\;\overline{\text{p2}}}$$

If particular variables must be replaced with their conjugate values, use the option `Conjugate`.

```mathematica
GA[\[Mu]] . (c1 GA[6] + c2 GA[7]) . GA[\[Nu]]
ComplexConjugate[%]
```

$$\bar{\gamma }^{\mu }.\left(\text{c1} \bar{\gamma }^6+\text{c2} \bar{\gamma }^7\right).\bar{\gamma }^{\nu }$$

$$\bar{\gamma }^{\nu }.\left(\text{c1} \bar{\gamma }^7+\text{c2} \bar{\gamma }^6\right).\bar{\gamma }^{\mu }$$

```mathematica
ComplexConjugate[GA[\[Mu]] . (c1 GA[6] + c2 GA[7]) . GA[\[Nu]], Conjugate -> {c1, c2}]
% // StandardForm
```

$$\bar{\gamma }^{\nu }.\left(\bar{\gamma }^7 \;\text{c1}^*+\bar{\gamma }^6 \;\text{c2}^*\right).\bar{\gamma }^{\mu }$$

```
(*DiracGamma[LorentzIndex[\[Nu]]] . (Conjugate[c2] DiracGamma[6] + Conjugate[c1] DiracGamma[7]) . DiracGamma[LorentzIndex[\[Mu]]]*)
```

It may happen that one needs to deal with amplitudes with amputated spinors, i.e. with open Dirac or Pauli indices. If the amplitude contains only a single chain of Dirac/Pauli matrices, everything remains unambiguous and the missing spinors are understood

```mathematica
GA[\[Mu], \[Nu], \[Rho], 5] CSI[i, j]
ComplexConjugate[%]
```

$$\overline{\sigma }^i.\overline{\sigma }^j \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^5$$

$$-\overline{\sigma }^j.\overline{\sigma }^i \bar{\gamma }^5.\bar{\gamma }^{\rho }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu }$$

However, when there are at least two spinor chains of the same type involved, such expressions do not make sense anymore. In these cases one should introduce explicit spinor indices to avoid ambiguities

```mathematica
DCHN[GA[\[Mu], \[Nu], \[Rho], 5], i, j] DCHN[GA[\[Mu], \[Nu], \[Rho], 5], k, l]
ComplexConjugate[%]
```

$$\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^5\right){}_{ij} \left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^5\right){}_{kl}$$

$$\left(\bar{\gamma }^5.\bar{\gamma }^{\text{\$AL}(\text{\$23})}.\bar{\gamma }^{\text{\$AL}(\text{\$24})}.\bar{\gamma }^{\text{\$AL}(\text{\$25})}\right){}_{ji} \left(\bar{\gamma }^5.\bar{\gamma }^{\text{\$AL}(\text{\$23})}.\bar{\gamma }^{\text{\$AL}(\text{\$24})}.\bar{\gamma }^{\text{\$AL}(\text{\$25})}\right){}_{lk}$$

```mathematica
PCHN[CSI[i, j, k], a, b] PCHN[CSI[i, j, k], c, d]
ComplexConjugate[%]
```

$$\left(\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k\right){}_{ab} \left(\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k\right){}_{cd}$$

$$\left(\overline{\sigma }^{\text{\$AL}(\text{\$26})}.\overline{\sigma }^{\text{\$AL}(\text{\$27})}.\overline{\sigma }^{\text{\$AL}(\text{\$28})}\right){}_{ba} \left(\overline{\sigma }^{\text{\$AL}(\text{\$26})}.\overline{\sigma }^{\text{\$AL}(\text{\$27})}.\overline{\sigma }^{\text{\$AL}(\text{\$28})}\right){}_{dc}$$

The function does not apply `Conjugate` to symbols that do not depend on `I` and are unrelated to Dirac/Pauli/Color matrices. One can specify symbols that need to be explicitly conjugated using the `Conjugate` option

```mathematica
cc SpinorU[p1] . GA[mu] . SpinorV[p2]
ComplexConjugate[%]
```

$$\text{cc} u(\text{p1}).\bar{\gamma }^{\text{mu}}.v(\text{p2})$$

$$\text{cc} \left(\varphi (-\overline{\text{p2}})\right).\bar{\gamma }^{\text{mu}}.\left(\varphi (\overline{\text{p1}})\right)$$

```mathematica
cc SpinorU[p1] . GA[mu] . SpinorV[p2]
ComplexConjugate[%, Conjugate -> {cc}] 
  
 

```

$$\text{cc} u(\text{p1}).\bar{\gamma }^{\text{mu}}.v(\text{p2})$$

$$\text{cc}^* \left(\varphi (-\overline{\text{p2}})\right).\bar{\gamma }^{\text{mu}}.\left(\varphi (\overline{\text{p1}})\right)$$