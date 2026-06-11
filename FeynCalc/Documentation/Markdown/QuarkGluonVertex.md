## QuarkGluonVertex

`QuarkGluonVertex[mu, a]` gives the Feynman rule for the quark-gluon vertex.

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