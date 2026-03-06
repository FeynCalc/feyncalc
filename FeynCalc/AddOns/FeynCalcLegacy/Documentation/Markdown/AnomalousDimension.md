## AnomalousDimension

`AnomalousDimension[name]` is a database of anomalous dimensions of twist 2 operators.

` AnomalousDimension["gnsqg0"]` yields the non-singlet one-loop contribution to the anomalous dimension $\gamma_{S,qg}^{(0),m}$ in the MS-bar scheme etc.

### See also

[Overview](Extra/FeynCalc.md), [SplittingFunction](SplittingFunction.md), [SumS](SumS.md), [SumT](SumT.md).

### Examples

Polarized case:

```mathematica
SetOptions[AnomalousDimension, Polarization -> 1]
```

$$\{\text{Polarization}\to 1,\text{Simplify}\to \;\text{FullSimplify}\}$$

$\gamma _{NS,qq }^{(0) }$ polarized:

```mathematica
AnomalousDimension[gnsqq0]
```

$$C_F \left(8 S_1(m-1)+\frac{4}{m}+\frac{4}{m+1}-6\right)$$

$\gamma _{S,qg }^{(0)}$ polarized:

```mathematica
AnomalousDimension[gsqg0]
```

$$\left(\frac{8}{m}-\frac{16}{m+1}\right) T_f$$

$\gamma _{S,gq }^{(0)}$polarized:

```mathematica
AnomalousDimension[gsgq0]
```

$$\left(\frac{4}{m+1}-\frac{8}{m}\right) C_F$$

$\gamma _{S,gg}^{(0)}$ polarized:

```mathematica
AnomalousDimension[gsgg0]
```

$$C_A \left(8 S_1(m-1)-\frac{8}{m}+\frac{16}{m+1}-\frac{22}{3}\right)+\frac{8 T_f}{3}$$

$\gamma _{PS,qq}^{(0)}$ polarized:

```mathematica
AnomalousDimension[gpsqq1]
```

$$16 \left(\frac{2}{m^3}-\frac{1}{m^2}+\frac{1}{m+1}+\frac{3}{(m+1)^2}+\frac{2}{(m+1)^3}-\frac{1}{m}\right) C_F T_f$$

$\gamma _{NS,qq }^{(1)}$ polarized:

```mathematica
AnomalousDimension[gnsqq1]
```

$$-C_A C_F \left(-\frac{16 \tilde{S}_2(m-1)}{m}-\frac{16 \tilde{S}_2(m-1)}{m+1}+16 \tilde{S}_3(m-1)-32 \tilde{S}_{12}(m-1)+\frac{44}{3 m^2}-\frac{536}{9} S_1(m-1)+\frac{88}{3} S_2(m-1)-16 S_3(m-1)+\frac{212}{9 m}-\frac{748}{9 (m+1)}-\frac{4}{3 (m+1)^2}-\frac{16}{(m+1)^3}+\frac{17}{3}\right)-\left(C_F^2 \left(\frac{32 \tilde{S}_2(m-1)}{m}+\frac{32 \tilde{S}_2(m-1)}{m+1}-32 \tilde{S}_3(m-1)+64 \tilde{S}_{12}(m-1)+\frac{8}{m^3}+\frac{16 S_1(m-1)}{m^2}+\frac{16 S_1(m-1)}{(m+1)^2}+\frac{16 S_2(m-1)}{m}+\frac{16 S_2(m-1)}{m+1}-24 S_2(m-1)+32 S_{12}(m-1)+32 S_{21}(m-1)-\frac{40}{m}+\frac{40}{m+1}+\frac{16}{(m+1)^2}+\frac{40}{(m+1)^3}+3\right)\right)-C_F N_f \left(-\frac{8}{3 m^2}+\frac{80}{9} S_1(m-1)-\frac{16}{3} S_2(m-1)-\frac{8}{9 m}+\frac{88}{9 (m+1)}-\frac{8}{3 (m+1)^2}-\frac{2}{3}\right)$$

$\gamma _{S,qg }^{(1)}$ polarized:

```mathematica
AnomalousDimension[gsqg1]
```

$$16 C_A T_f \left(-\frac{2 \tilde{S}_2(m-1)}{m}+\frac{4 \tilde{S}_2(m-1)}{m+1}+\frac{2}{m^3}-\frac{2 S_1(m-1)}{m^2}-\frac{3}{m^2}-\frac{S_1^2(m-1)}{m}+\frac{2 S_1^2(m-1)}{m+1}+\frac{4 S_1(m-1)}{(m+1)^2}-\frac{S_2(m-1)}{m}+\frac{2 S_2(m-1)}{m+1}-\frac{4}{m}+\frac{3}{m+1}+\frac{8}{(m+1)^2}+\frac{12}{(m+1)^3}\right)+8 C_F T_f \left(-\frac{2}{m^3}-\frac{1}{m^2}+\frac{2 S_1^2(m-1)}{m}-\frac{4 S_1^2(m-1)}{m+1}-\frac{2 S_2(m-1)}{m}+\frac{4 S_2(m-1)}{m+1}+\frac{14}{m}-\frac{19}{m+1}-\frac{8}{(m+1)^2}+\frac{4}{(m+1)^3}\right)$$

$\gamma _{S,gq }^{(1)}$ polarized:

```mathematica
AnomalousDimension[gsgq1]
```

$$8 C_A C_F \left(\frac{4 \tilde{S}_2(m-1)}{m}-\frac{2 \tilde{S}_2(m-1)}{m+1}-\frac{4}{m^3}+\frac{28}{3 m^2}-\frac{2 S_1^2(m-1)}{m}+\frac{S_1^2(m-1)}{m+1}+\frac{16 S_1(m-1)}{3 m}-\frac{5 S_1(m-1)}{3 (m+1)}+\frac{2 S_2(m-1)}{m}-\frac{S_2(m-1)}{m+1}-\frac{56}{9 m}-\frac{20}{9 (m+1)}-\frac{38}{3 (m+1)^2}-\frac{6}{(m+1)^3}\right)+32 C_F T_f \left(-\frac{2}{3 m^2}-\frac{2 S_1(m-1)}{3 m}+\frac{S_1(m-1)}{3 (m+1)}+\frac{7}{9 m}-\frac{2}{9 (m+1)}+\frac{1}{3 (m+1)^2}\right)+4 C_F^2 \left(\frac{4}{m^3}+\frac{8 S_1(m-1)}{m^2}-\frac{12}{m^2}+\frac{4 S_1^2(m-1)}{m}-\frac{2 S_1^2(m-1)}{m+1}-\frac{8 S_1(m-1)}{m}+\frac{2 S_1(m-1)}{m+1}-\frac{4 S_1(m-1)}{(m+1)^2}+\frac{4 S_2(m-1)}{m}-\frac{2 S_2(m-1)}{m+1}+\frac{15}{m}-\frac{6}{m+1}+\frac{3}{(m+1)^2}-\frac{2}{(m+1)^3}\right)$$

$\gamma _{S,gg }^{(1)}$ polarized:

```mathematica
v1 = AnomalousDimension[gsgg1]
```

$$4 C_A^2 \left(\frac{8 \tilde{S}_2(m-1)}{m}-\frac{16 \tilde{S}_2(m-1)}{m+1}+4 \tilde{S}_3(m-1)-8 \tilde{S}_{12}(m-1)-\frac{8}{m^3}+\frac{8 S_1(m-1)}{m^2}+\frac{58}{3 m^2}-\frac{16 S_1(m-1)}{(m+1)^2}+\frac{134}{9} S_1(m-1)+\frac{8 S_2(m-1)}{m}-\frac{16 S_2(m-1)}{m+1}+4 S_3(m-1)-8 S_{12}(m-1)-8 S_{21}(m-1)-\frac{107}{9 m}+\frac{241}{9 (m+1)}-\frac{86}{3 (m+1)^2}-\frac{48}{(m+1)^3}-\frac{16}{3}\right)+32 C_A T_f \left(-\frac{1}{3 m^2}-\frac{5}{9} S_1(m-1)+\frac{14}{9 m}-\frac{19}{9 (m+1)}-\frac{1}{3 (m+1)^2}+\frac{1}{3}\right)+8 \left(\frac{4}{m^3}-\frac{10}{m^2}-\frac{10}{m+1}+\frac{2}{(m+1)^2}+\frac{4}{(m+1)^3}+\frac{10}{m}+1\right) C_F T_f$$

$\gamma _{S,gg }^{(1)}$ polarized (different representation):

```mathematica
v2 = AnomalousDimension[GSGG1];
```

Check that all odd moments give the same for the two representations of $\gamma _{S,gg }^{(1)}$:

```mathematica
Table[v1 - v2 /. OPEm -> ij, {ij, 1, 17, 2}] // Simplify
```

$$\{0,0,0,0,0,0,0,0,0\}$$