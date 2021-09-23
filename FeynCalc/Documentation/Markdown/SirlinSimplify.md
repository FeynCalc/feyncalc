## SirlinSimplify

`SirlinSimplify[exp]` simplifies spinor chains that contain Dirac matrices using relations derived by A. Sirlin in [Nuclear Physics B192 (1981) 93-99](https://doi.org/10.1016/0550-3213(81)90195-4). Contrary to the original paper, the sign of the Levi-Civita tensor is chosen as $\varepsilon^{0123}=1$ which is the standard choice in FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md), [SpinorChainTrick](SpinorChainTrick.md).

### Examples

```mathematica
SpinorUBar[p3, m3] . GA[\[Mu], \[Rho], \[Nu], 7] . SpinorU[p1, m1] SpinorUBar[p4, m4] . GA[\[Mu], \[Tau], \[Nu], 7] . SpinorU[p2, m2]
SirlinSimplify[%]
```

$$\bar{u}(\text{p3},\text{m3}).\bar{\gamma }^{\mu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\nu }.\bar{\gamma }^7.u(\text{p1},\text{m1}) \bar{u}(\text{p4},\text{m4}).\bar{\gamma }^{\mu }.\bar{\gamma }^{\tau }.\bar{\gamma }^{\nu }.\bar{\gamma }^7.u(\text{p2},\text{m2})$$

$$4 \bar{g}^{\rho \tau } \left(\varphi (\overline{\text{p3}},\text{m3})\right).\bar{\gamma }^{\text{liS33}}.\bar{\gamma }^7.\left(\varphi (\overline{\text{p1}},\text{m1})\right) \left(\varphi (\overline{\text{p4}},\text{m4})\right).\bar{\gamma }^{\text{liS33}}.\bar{\gamma }^7.\left(\varphi (\overline{\text{p2}},\text{m2})\right)$$