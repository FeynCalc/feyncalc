## EpsChisholm

`EpsChisholm[exp]` applies the Chisholm identity to a Dirac matrix contracted with a Levi-Civita tensor.

### See also

[Overview](Extra/FeynCalc.md), [Chisholm](Chisholm.md), [Eps](Eps.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]] GA[\[Sigma], 5]
EpsChisholm[%]
```

$$\bar{\gamma }^{\sigma }.\bar{\gamma }^5 \bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$-i \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }+i \bar{\gamma }^{\rho } \bar{g}^{\mu \nu }-i \bar{\gamma }^{\nu } \bar{g}^{\mu \rho }+i \bar{\gamma }^{\mu } \bar{g}^{\nu \rho }$$

This reproduces the identities given in the Appendix A of (arXiv:2111.05153)[https://arxiv.org/abs/2111.05153]

```mathematica
LC[\[Alpha], \[Nu], \[Beta], \[Rho]] FV[Subscript[p, 1], \[Beta]] SpinorUBar[Subscript[p, 2], SMP["m_s"]] . GA[\[Alpha], 7] . SpinorV[Subscript[p, 1], SMP["m_d"]]
% // EpsChisholm // DiracSimplify // Contract
```

$$\overline{p}_1{}^{\beta } \bar{\epsilon }^{\alpha \nu \beta \rho } \bar{u}\left(p_2,m_s\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^7.v\left(p_1,m_d\right)$$

$$i m_d \bar{g}^{\nu \rho } \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^6.\left(\varphi (-\overline{p}_1,m_d)\right)+i \overline{p}_1{}^{\nu } \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^{\rho }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_1,m_d)\right)-i \overline{p}_1{}^{\rho } \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^{\nu }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_1,m_d)\right)-i m_d \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^6.\left(\varphi (-\overline{p}_1,m_d)\right)$$

```mathematica
LC[\[Alpha], \[Nu], \[Beta], \[Rho]] FV[Subscript[p, 2], \[Beta]] SpinorUBar[Subscript[p, 2], SMP["m_s"]] . GA[\[Alpha], 7] . SpinorV[Subscript[p, 1], SMP["m_d"]]
% // EpsChisholm // DiracSimplify // Contract
```

$$\overline{p}_2{}^{\beta } \bar{\epsilon }^{\alpha \nu \beta \rho } \bar{u}\left(p_2,m_s\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^7.v\left(p_1,m_d\right)$$

$$-i m_s \bar{g}^{\nu \rho } \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^7.\left(\varphi (-\overline{p}_1,m_d)\right)-i \overline{p}_2{}^{\nu } \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^{\rho }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_1,m_d)\right)+i \overline{p}_2{}^{\rho } \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^{\nu }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_1,m_d)\right)+i m_s \left(\varphi (\overline{p}_2,m_s)\right).\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_1,m_d)\right)$$

```mathematica
LC[\[Alpha], \[Nu], \[Gamma], \[Rho]] FV[Subscript[p, 3], \[Gamma]] SpinorUBar[Subscript[p, 3], SMP["m_s"]] . GA[\[Nu], 7] . SpinorV[Subscript[p, 4], SMP["m_d"]]
% // EpsChisholm // DiracSimplify // Contract
```

$$\overline{p}_3{}^{\gamma } \bar{\epsilon }^{\alpha \nu \gamma \rho } \bar{u}\left(p_3,m_s\right).\bar{\gamma }^{\nu }.\bar{\gamma }^7.v\left(p_4,m_d\right)$$

$$i m_s \bar{g}^{\alpha \rho } \left(\varphi (\overline{p}_3,m_s)\right).\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)+i \overline{p}_3{}^{\alpha } \left(\varphi (\overline{p}_3,m_s)\right).\bar{\gamma }^{\rho }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)-i \overline{p}_3{}^{\rho } \left(\varphi (\overline{p}_3,m_s)\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)-i m_s \left(\varphi (\overline{p}_3,m_s)\right).\bar{\gamma }^{\alpha }.\bar{\gamma }^{\rho }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)$$

```mathematica
LC[\[Beta], \[Gamma], \[Mu], \[Nu]] FV[Subscript[p, 2], \[Gamma]] SpinorUBar[Subscript[p, 3], SMP["m_s"]] . GA[\[Beta], 7] . SpinorV[Subscript[p, 4], SMP["m_d"]]
% // EpsChisholm // DiracSimplify // Contract 
  
 

```

$$\overline{p}_2{}^{\gamma } \bar{\epsilon }^{\beta \gamma \mu \nu } \bar{u}\left(p_3,m_s\right).\bar{\gamma }^{\beta }.\bar{\gamma }^7.v\left(p_4,m_d\right)$$

$$i \bar{g}^{\mu \nu } \left(\varphi (\overline{p}_3,m_s)\right).\left(\bar{\gamma }\cdot \overline{p}_2\right).\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)+i \overline{p}_2{}^{\mu } \left(\varphi (\overline{p}_3,m_s)\right).\bar{\gamma }^{\nu }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)-i \overline{p}_2{}^{\nu } \left(\varphi (\overline{p}_3,m_s)\right).\bar{\gamma }^{\mu }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)-i \left(\varphi (\overline{p}_3,m_s)\right).\left(\bar{\gamma }\cdot \overline{p}_2\right).\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^7.\left(\varphi (-\overline{p}_4,m_d)\right)$$