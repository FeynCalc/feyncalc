## QuarkGluonVertex

`QuarkGluonVertex[μ, a]` gives the Feynman rule for the quark-gluon vertex.

`QGV` can be used as an abbreviation of `QuarkGluonVertex`.

The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`.

### See also

[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md).

### Examples

```mathematica
QuarkGluonVertex[\[Mu], a, Explicit -> True]
```

$$i g_s T^a.\gamma ^{\mu }$$

```mathematica
QGV[\[Mu], a]
```

$$Q_a^{\mu }$$

```mathematica
Explicit[%]
```

$$i g_s T^a.\gamma ^{\mu }$$

```mathematica
QuarkGluonVertex[\[Mu], a, CounterTerm -> 1, Explicit -> True]
```

$$\frac{2 i g_s^3 S_n \left(C_F-\frac{C_A}{2}\right) T^a.\gamma ^{\mu }}{\varepsilon }$$

```mathematica
QuarkGluonVertex[\[Mu], a, CounterTerm -> 2, Explicit -> True]
```

$$\frac{3 i C_A g_s^3 S_n T^a.\gamma ^{\mu }}{\varepsilon }$$

```mathematica
QuarkGluonVertex[\[Mu], a, CounterTerm -> 3, Explicit -> True]
```

$$\frac{2 i g_s^3 S_n \left(C_A+C_F\right) T^a.\gamma ^{\mu }}{\varepsilon }$$

```mathematica
QuarkGluonVertex[{p, \[Mu], a}, {q}, {k}, OPE -> True, Explicit -> True]
```

$$\Omega  \Delta ^{\mu } g_s (\gamma \cdot \Delta ).T^a \left(\sum _{i=0}^{-2+m} (-1)^i (k\cdot \Delta )^i (\Delta \cdot q)^{-2-i+m}\right)+i g_s T^a.\gamma ^{\mu }$$

```mathematica
QuarkGluonVertex[{p, \[Mu], a}, {q}, {k}, OPE -> False, Explicit -> True]
```

$$i g_s T^a.\gamma ^{\mu }$$
