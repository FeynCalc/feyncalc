## FCMatrixIsolate

`FCMatrixIsolate[exp]` wraps the occurring Dirac, Pauli and color objects into heads specified by the user.

### See also

[Overview](Extra/FeynCalc.md), [FCDiracIsolate](FCDiracIsolate.md), [FCColorIsolate](FCColorIsolate.md), [FCPauliIsolate](FCPauliIsolate.md).

### Examples

```mathematica
ex = -e eQ gs Spinor[Momentum[k2], mu, 1] . GS[Polarization[k1, -I, 
       Transversality -> True]] . (mu + GS[k1 + k2]) . GS[Polarization[p2, 
       I]] . Spinor[Momentum[p1], mu, 1] FAD[{-k1 - k2, mu}, Dimension -> 4]*
    SUNTF[{Glu3}, Col4, Col1] - e eQ gs DCHN[Spinor[Momentum[k2], mu, 
      1], i] DCHN[GS[Polarization[p2, I]] . (mu + GS[k2 - p2]) . GS[Polarization[k1, 
        -I, Transversality -> True]], i, j] DCHN[Spinor[Momentum[p1], mu, 1], j]*
    FAD[{-k2 + p2, mu}, Dimension -> 4] SUNTF[{Glu3}, Col4, Col1]
```

$$-\frac{e \;\text{eQ} \;\text{gs} T_{\text{Col4}\;\text{Col1}}^{\text{Glu3}} \left(\varphi (\overline{\text{k2}},\text{mu})\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k1})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}+\overline{\text{k2}}\right)+\text{mu}\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{p2})\right).\left(\varphi (\overline{\text{p1}},\text{mu})\right)}{(-\overline{\text{k1}}-\overline{\text{k2}})^2-\text{mu}^2}-\frac{e \;\text{eQ} \;\text{gs} T_{\text{Col4}\;\text{Col1}}^{\text{Glu3}} \left(\varphi (\overline{\text{k2}},\text{mu})\right)_i \left(\varphi (\overline{\text{p1}},\text{mu})\right)_j \left(\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{p2})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)+\text{mu}\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k1})\right)\right){}_{ij}}{(\overline{\text{p2}}-\overline{\text{k2}})^2-\text{mu}^2}$$

```mathematica
FCMatrixIsolate[ex, FCDiracIsolate -> {dch}, FCColorIsolate -> {cch}, 
  FCPauliIsolate -> {pch}, Head -> re, FCE -> True]
```

$$\text{cch}\left(T_{\text{Col4}\;\text{Col1}}^{\text{Glu3}}\right) \;\text{re}\left(-\frac{e \;\text{eQ} \;\text{gs}}{(\overline{\text{p2}}-\overline{\text{k2}})^2-\text{mu}^2}\right) \;\text{dch}\left(\left(\varphi (\overline{\text{k2}},\text{mu})\right)_i \left(\varphi (\overline{\text{p1}},\text{mu})\right)_j \left(\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{p2})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k2}}-\overline{\text{p2}}\right)+\text{mu}\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k1})\right)\right){}_{ij}\right)+\text{cch}\left(T_{\text{Col4}\;\text{Col1}}^{\text{Glu3}}\right) \;\text{re}\left(-\frac{e \;\text{eQ} \;\text{gs}}{(-\overline{\text{k1}}-\overline{\text{k2}})^2-\text{mu}^2}\right) \;\text{dch}\left(\left(\varphi (\overline{\text{k2}},\text{mu})\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*(\text{k1})\right).\left(\bar{\gamma }\cdot \left(\overline{\text{k1}}+\overline{\text{k2}}\right)+\text{mu}\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(\text{p2})\right).\left(\varphi (\overline{\text{p1}},\text{mu})\right)\right)$$
