## SplittingFunction

`SplittingFunction[pxy]` is a database of splitting functions in the $\overline{\textrm{MS}}$ scheme.

`SplittingFunction["Pqq", x]`, `SplittingFunction["Pqg", x]`, `SplittingFunction["Pgq", x]`  and `SplittingFunction["Pgg", x]` yield the lowest order splitting functions.

`SplittingFunction["PQQS",x]`, `SplittingFunction["PQQNS",x]` and `SplittingFunction["PQG",x]` are the next to leading order splitting functions.

SplittingFunction has an option Polarization.

`SplittingFunction["Pqq", x, Polarization -> 0]` returns the unpolarized and `SplittingFunction["Pqq", x, Polarization -> 1]` the polarized splitting functions.

### See also

[Overview](Extra/FeynCalc.md), [AnomalousDimension](AnomalousDimension.md).

### Examples

Unpolarized case:

In general the argument should be a string, but if the variables Pqq etc. have no value, you can omit the "".

```mathematica
SplittingFunction[Pqq, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

$$C_F \left(6 \delta (1-x)-4 x+8 \left(\frac{1}{1-x}\right)_+-4\right)$$

```mathematica
SplittingFunction[Pqg, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

$$\left(16 x^2-16 x+8\right) T_f$$

```mathematica
SplittingFunction[Pgq, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

$$\left(4 x+\frac{8}{x}-8\right) C_F$$

```mathematica
SplittingFunction[Pgg, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

$$8 C_A \left(-x^2+\frac{11}{12} \delta (1-x)+x+\left(\frac{1}{1-x}\right)_++\frac{1}{x}-2\right)-\frac{8}{3} N_f T_f \delta (1-x)$$

```mathematica
SplittingFunction[aqq, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

$$C_F \left((7-4 \zeta (2)) \delta (1-x)+2 x+(2 x+2) \log ((1-x) x)-4 \left(\frac{\log (x)}{1-x}+\left(\frac{\log (1-x)}{1-x}\right)_+\right)-4\right)$$

```mathematica
SplittingFunction[agq, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

$$C_F \left(-2 x-\frac{4}{x}+\left(-2 x-\frac{4}{x}+4\right) \log ((1-x) x)+2\right)$$

```mathematica
SplittingFunction[aqg, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

$$T_f \left(\left(-8 x^2+8 x-4\right) \log ((1-x) x)-4\right)$$

```mathematica
SplittingFunction[agg, Polarization -> 0] /. FCGV[z_] :> ToExpression[z]
```

Polarized case:

```mathematica
SplittingFunction[Pqq, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_F \left(6 \delta (1-x)-4 x+8 \left(\frac{1}{1-x}\right)_+-4\right)$$

```mathematica
SplittingFunction[Pqg, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$(16 x-8) T_f$$

```mathematica
SplittingFunction[Pgq, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$(8-4 x) C_F$$

```mathematica
SplittingFunction[Pgg, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_A \left(\frac{22}{3} \delta (1-x)-16 x+8 \left(\frac{1}{1-x}\right)_++8\right)-\frac{8}{3} N_f T_f \delta (1-x)$$

```mathematica
SplittingFunction[aqq, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_F \left((7-4 \zeta (2)) \delta (1-x)+8 (1-x)+2 x+(2 x+2) \log ((1-x) x)-4 \left(\frac{1}{1-x}\right)_+ \log (x)-4 \left(\frac{\log (1-x)}{1-x}\right)_+-4\right)$$

```mathematica
SplittingFunction[agq, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_F (-4 x+(2 x-4) \log ((1-x) x)+2)$$

```mathematica
SplittingFunction[agqd, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_F ((2 x-4) \log ((1-x) x)-2)$$

```mathematica
SplittingFunction[aqg, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$T_f ((4-8 x) \log ((1-x) x)-4)$$

```mathematica
SplittingFunction[aqgd, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$T_f ((4-8 x) \log ((1-x) x)-4)$$

```mathematica
SplittingFunction[agg, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_A \left(\left(\frac{67}{9}-4 \zeta (2)\right) \delta (1-x)+(8 x-4) \log ((1-x) x)-4 \left(\frac{\log (x)}{1-x}+\left(\frac{\log (1-x)}{1-x}\right)_+\right)+2\right)-\frac{20}{9} T_f \delta (1-x)$$

```mathematica
SplittingFunction[aggd, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_A \left(\left(\frac{67}{9}-4 \zeta (2)\right) \delta (1-x)+(8 x-4) \log ((1-x) x)-4 \left(\frac{\log (x)}{1-x}+\left(\frac{\log (1-x)}{1-x}\right)_+\right)+2\right)-\frac{20}{9} T_f \delta (1-x)$$

```mathematica
SplittingFunction[PQQS, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_F T_f \left(16 (1-x)-16 (x+1) \log ^2(x)+(48 x-16) \log (x)\right)$$

```mathematica
SplittingFunction[PQQNS, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$-8 C_F \left(C_F-\frac{C_A}{2}\right) \left(\frac{\left(x^2+1\right) \left(-2 \zeta (2)-4 \;\text{Li}_2(-x)+\log ^2(x)-4 \log (x+1) \log (x)\right)}{x+1}+4 (1-x)+2 (x+1) \log (x)\right)+C_A C_F \left(\frac{4 \left(x^2+1\right) \log ^2(x)}{1-x}+8 \zeta (2) (x+1)+\left(\frac{536}{9}-16 \zeta (2)\right) \left(\frac{1}{1-x}\right)_++\delta (1-x) \left(\frac{88 \zeta (2)}{3}-24 \zeta (3)+\frac{17}{3}\right)+\frac{4}{9} (53-187 x)-\frac{4}{3} \left(5 x-\frac{22}{1-x}+5\right) \log (x)\right)+C_F N_f \left(-\frac{8 \left(x^2+1\right) \log (x)}{3 (1-x)}+\left(-\frac{16 \zeta (2)}{3}-\frac{2}{3}\right) \delta (1-x)+\frac{88 x}{9}-\frac{80}{9} \left(\frac{1}{1-x}\right)_+-\frac{8}{9}\right)+C_F^2 \left(-\frac{16 \left(x^2+1\right) \log (1-x) \log (x)}{1-x}+\delta (1-x) (-24 \zeta (2)+48 \zeta (3)+3)-40 (1-x)-4 (x+1) \log ^2(x)-8 \left(2 x+\frac{3}{1-x}\right) \log (x)\right)$$

```mathematica
SplittingFunction[PQG, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$4 C_A T_f \left(-8 \zeta (2)+(-16 x-8) \;\text{Li}_2(-x)-44 x+(4-8 x) \log ^2(1-x)+(-8 x-4) \log ^2(x)+(16 x-16) \log (1-x)+(32 x+4) \log (x)+(-16 x-8) \log (x) \log (x+1)+48\right)+4 C_F T_f \left(8 \zeta (2)-16 \zeta (2) x+54 x+(8 x-4) \log ^2(1-x)+(4 x-2) \log ^2(x)+(16-16 x) \log (1-x)+(8-16 x) \log (x) \log (1-x)-18 \log (x)-44\right)$$

```mathematica
SplittingFunction[PGQ, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$\left(C_A C_F\right).\left((16 x+32) \;\text{Li}_2(-x)+16 \zeta (2) x+\frac{280 x}{9}+(16-8 x) \log ^2(1-x)+(8 x+16) \log ^2(x)+\left(\frac{8 x}{3}+\frac{80}{3}\right) \log (1-x)+(16 x-32) \log (x) \log (1-x)+(32-104 x) \log (x)+(16 x+32) \log (x) \log (x+1)+\frac{328}{9}\right)+\left(C_F T_f\right).\left(-\frac{32 x}{9}+\left(\frac{32 x}{3}-\frac{64}{3}\right) \log (1-x)-\frac{128}{9}\right)+C_F^2.\left(32 x+(8 x-16) \log ^2(1-x)+(8-4 x) \log ^2(x)+(-8 x-16) \log (1-x)-(32 x+64) \log (x)+(36 x+48) \log (x)-68\right)$$

```mathematica
SplittingFunction[PGG, Polarization -> 1] /. FCGV[z_] :> ToExpression[z]
```

$$C_A T_f \left(-\frac{32}{3} \delta (1-x)+\frac{608 x}{9}-\frac{160}{9} \left(\frac{1}{1-x}\right)_++\left(-\frac{32 x}{3}-\frac{32}{3}\right) \log (x)-\frac{448}{9}\right)+C_A^2 \left(\left(64 x+\frac{32}{x+1}+32\right) \;\text{Li}_2(-x)+\frac{64}{3} \delta (1-x)+\zeta (2) \left(64 x-16 \left(\frac{1}{1-x}\right)_++\frac{16}{x+1}\right)+24 \zeta (3) \delta (1-x)-\frac{388 x}{9}+\frac{536}{9} \left(\frac{1}{1-x}\right)_++\left(-\frac{8}{x+1}+\frac{8}{1-x}+32\right) \log ^2(x)+\left(\frac{232}{3}-\frac{536 x}{3}\right) \log (x)+\left(64 x-\frac{32}{1-x}-32\right) \log (1-x) \log (x)+\left(64 x+\frac{32}{x+1}+32\right) \log (x+1) \log (x)-\frac{148}{9}\right)+C_F T_f \left(-8 \delta (1-x)+80 x+(-16 x-16) \log ^2(x)+(16 x-80) \log (x)-80\right)$$
