## FCAbbreviate

`FCAbbreviate[exp, {q1, q2, ...}, {p1, p2, ...}]` introduces abbreivations for scalar products of external momenta, `SMP`-symbols and other variables that are present in the expression. Functions (`LeafCount > 1`) are not supported. The main purpose is to simplify the export of FeynCalc expressions to other software tools that might not provide the richness of Mathematica's syntax. The result is returned as a list of replacement rules for scalar products, `SMP`s and all other variables present.

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [SMP](SMP.md).

### Examples

```mathematica
(a + I b)^2
FCAbbreviate[%, {}, {}]
```

$$(a+i b)^2$$

$$\{\{\},\{\},\{a\to \;\text{var1},b\to \;\text{var2}\}\}$$

```mathematica
SPD[p, k] FAD[{q, SMP["m_e"]}, {q + p, m}]
FCAbbreviate[%, {q}, {p, k}, Head -> spd]
```

$$\frac{k\cdot p}{\left(q^2-m_e^2\right).\left((p+q)^2-m^2\right)}$$

$$\left\{\{\text{spd}(k,k)\to \;\text{sp1},\text{spd}(k,p)\to \;\text{sp2},\text{spd}(p,p)\to \;\text{sp3}\},\left\{m_e\to \;\text{sm1}\right\},\{m\to \;\text{var1}\}\right\}$$

```mathematica
FCClearScalarProducts[]; 
SPD[p1, p1] = 0;
SPD[p2, p2] = 0;
SPD[p3, p3] = 0;
SPD[p1, p2] = s/2; SPD[p1, p3] = -(s + t)/2; SPD[p2, p3] = t/2;
SPD[p2, p3] FAD[q, q - p1 - p2, q - p1 - p2 - p3]
FCAbbreviate[%, {q}, {p1, p2, p3}, Head -> spd]
```

$$\frac{t}{2 q^2.(-\text{p1}-\text{p2}+q)^2.(-\text{p1}-\text{p2}-\text{p3}+q)^2}$$

$$\left\{\left\{\text{spd}(\text{p1},\text{p1})\to 0,\text{spd}(\text{p1},\text{p2})\to \frac{\text{var1}}{2},\text{spd}(\text{p1},\text{p3})\to \frac{1}{2} (-\text{var1}-\text{var2}),\text{spd}(\text{p2},\text{p2})\to 0,\text{spd}(\text{p2},\text{p3})\to \frac{\text{var2}}{2},\text{spd}(\text{p3},\text{p3})\to 0\right\},\{\},\{s\to \;\text{var1},t\to \;\text{var2}\}\right\}$$

```mathematica
FCClearScalarProducts[]
```
