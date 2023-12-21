## FCDiracIsolate

`FCDiracIsolate[exp]` wraps chains of Dirac matrices into heads specified by the user.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md).

### Examples

`FCDiracIsolate` provides an easy way to extract the Dirac structures present in the expression (e.g. an amplitude)

```mathematica
amp = (Spinor[Momentum[p2], SMP["m_u"], 1] . (-I GA[\[Mu]] SMP["g_s"] SUNTF[{Glu2}, 
         Col3, Col5]) . (GS[-k1 + p2] + SMP["m_u"]) . (-I GA[\[Nu]] SMP["g_s"] SUNTF[{Glu4}, 
         Col5, Col1]) . Spinor[Momentum[p1], SMP["m_u"], 1] FAD[{k1 - p2, 
       SMP["m_u"]}, Dimension -> 4] FV[Polarization[k1, I], \[Mu]] FV[Polarization[k2, -I], 
      \[Nu]] + Spinor[Momentum[p2], SMP["m_u"], 1] . (-I GA[\[Nu]] SMP["g_s"] SUNTF[{Glu4}, 
         Col3, Col5]) . (GS[k2 + p2] + SMP["m_u"]) . (-I GA[\[Mu]] SMP["g_s"] SUNTF[{Glu2}, 
         Col5, Col1]) . Spinor[Momentum[p1], SMP["m_u"], 1] FAD[{-k2 - p2, SMP["m_u"]}, 
      Dimension -> 4] FV[Polarization[k1, I], \[Mu]] FV[Polarization[k2, 
       -I], \[Nu]] - Spinor[Momentum[p2], SMP["m_u"], 1] . (-I GA[Lor3] SMP["g_s"] SUNTF[{Glu5}, 
         Col3, Col1]) . Spinor[Momentum[p1], SMP["m_u"], 1] FAD[-k1 + k2, 
      Dimension -> 4] FV[Polarization[k1, I], \[Mu]] FV[Polarization[k2, -I], 
      \[Nu]] MT[Lor3, Lor4] (FV[2 k1 - k2, \[Nu]] MT[Lor4, \[Mu]] + FV[-k1 + 2 k2, \[Mu]] MT[Lor4, 
         \[Nu]] + FV[-k1 - k2, Lor4] MT[\[Mu], \[Nu]]) SMP["g_s"] SUNF[Glu2, Glu4, Glu5])
```

$$\frac{\bar{\varepsilon }^{\mu }(\text{k1}) \bar{\varepsilon }^{*\nu }(\text{k2}) \left(\varphi (\overline{\text{p2}},m_u)\right).\left(-i g_s \bar{\gamma }^{\mu } T_{\text{Col3}\;\text{Col5}}^{\text{Glu2}}\right).\left(\bar{\gamma }\cdot \left(\overline{\text{p2}}-\overline{\text{k1}}\right)+m_u\right).\left(-i g_s \bar{\gamma }^{\nu } T_{\text{Col5}\;\text{Col1}}^{\text{Glu4}}\right).\left(\varphi (\overline{\text{p1}},m_u)\right)}{(\overline{\text{k1}}-\overline{\text{p2}})^2-m_u^2}+\frac{\bar{\varepsilon }^{\mu }(\text{k1}) \bar{\varepsilon }^{*\nu }(\text{k2}) \left(\varphi (\overline{\text{p2}},m_u)\right).\left(-i g_s \bar{\gamma }^{\nu } T_{\text{Col3}\;\text{Col5}}^{\text{Glu4}}\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}+\overline{\text{p2}}\right)+m_u\right).\left(-i g_s \bar{\gamma }^{\mu } T_{\text{Col5}\;\text{Col1}}^{\text{Glu2}}\right).\left(\varphi (\overline{\text{p1}},m_u)\right)}{(-\overline{\text{k2}}-\overline{\text{p2}})^2-m_u^2}-\frac{1}{(\overline{\text{k2}}-\overline{\text{k1}})^2}g_s \bar{g}^{\text{Lor3}\;\text{Lor4}} \bar{\varepsilon }^{\mu }(\text{k1}) \bar{\varepsilon }^{*\nu }(\text{k2}) f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} \left(\bar{g}^{\text{Lor4}\mu } \left(2 \overline{\text{k1}}-\overline{\text{k2}}\right)^{\nu }+\bar{g}^{\text{Lor4}\nu } \left(2 \overline{\text{k2}}-\overline{\text{k1}}\right)^{\mu }+\bar{g}^{\mu \nu } \left(-\overline{\text{k1}}-\overline{\text{k2}}\right)^{\text{Lor4}}\right) \left(\varphi (\overline{\text{p2}},m_u)\right).\left(-i g_s \bar{\gamma }^{\text{Lor3}} T_{\text{Col3}\;\text{Col1}}^{\text{Glu5}}\right).\left(\varphi (\overline{\text{p1}},m_u)\right)$$

```mathematica
ampIso = FCDiracIsolate[amp, Head -> diracS]
```

$$-\frac{g_s^2 \bar{\varepsilon }^{\mu }(\text{k1}) \bar{\varepsilon }^{*\nu }(\text{k2}) T_{\text{Col5}\;\text{Col1}}^{\text{Glu2}} T_{\text{Col3}\;\text{Col5}}^{\text{Glu4}} \;\text{diracS}\left(\left(\varphi (\overline{\text{p2}},m_u)\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}+\overline{\text{p2}}\right)+m_u\right).\bar{\gamma }^{\mu }.\left(\varphi (\overline{\text{p1}},m_u)\right)\right)}{(-\overline{\text{k2}}-\overline{\text{p2}})^2-m_u^2}-\frac{g_s^2 \bar{\varepsilon }^{\mu }(\text{k1}) \bar{\varepsilon }^{*\nu }(\text{k2}) T_{\text{Col5}\;\text{Col1}}^{\text{Glu4}} T_{\text{Col3}\;\text{Col5}}^{\text{Glu2}} \;\text{diracS}\left(\left(\varphi (\overline{\text{p2}},m_u)\right).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \left(\overline{\text{p2}}-\overline{\text{k1}}\right)+m_u\right).\bar{\gamma }^{\nu }.\left(\varphi (\overline{\text{p1}},m_u)\right)\right)}{(\overline{\text{k1}}-\overline{\text{p2}})^2-m_u^2}+\frac{1}{(\overline{\text{k2}}-\overline{\text{k1}})^2}i g_s^2 \bar{g}^{\text{Lor3}\;\text{Lor4}} \bar{\varepsilon }^{\mu }(\text{k1}) \bar{\varepsilon }^{*\nu }(\text{k2}) T_{\text{Col3}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} \left(\bar{g}^{\mu \nu } \left(-\left(\overline{\text{k1}}+\overline{\text{k2}}\right)^{\text{Lor4}}\right)-\bar{g}^{\text{Lor4}\nu } \left(\overline{\text{k1}}-2 \overline{\text{k2}}\right)^{\mu }+\bar{g}^{\text{Lor4}\mu } \left(2 \overline{\text{k1}}-\overline{\text{k2}}\right)^{\nu }\right) \;\text{diracS}\left(\left(\varphi (\overline{\text{p2}},m_u)\right).\bar{\gamma }^{\text{Lor3}}.\left(\varphi (\overline{\text{p1}},m_u)\right)\right)$$

Now that all Dirac structures are wrapped into the head `diracS` it is easy to extract them to a separate list

```mathematica
Cases2[ampIso, diracS]
```

$$\left\{\text{diracS}\left(\left(\varphi (\overline{\text{p2}},m_u)\right).\bar{\gamma }^{\text{Lor3}}.\left(\varphi (\overline{\text{p1}},m_u)\right)\right),\text{diracS}\left(\left(\varphi (\overline{\text{p2}},m_u)\right).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \left(\overline{\text{p2}}-\overline{\text{k1}}\right)+m_u\right).\bar{\gamma }^{\nu }.\left(\varphi (\overline{\text{p1}},m_u)\right)\right),\text{diracS}\left(\left(\varphi (\overline{\text{p2}},m_u)\right).\bar{\gamma }^{\nu }.\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}+\overline{\text{p2}}\right)+m_u\right).\bar{\gamma }^{\mu }.\left(\varphi (\overline{\text{p1}},m_u)\right)\right)\right\}$$

This way we obtain a sorted list of all unique Dirac structures in `amp`.

```mathematica
ClearAll[amp, ampIso, diracS]
```