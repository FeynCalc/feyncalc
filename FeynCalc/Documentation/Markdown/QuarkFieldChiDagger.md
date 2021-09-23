## QuarkFieldChiDagger

`QuarkFieldChiDagger` is the name of a fermionic field. This is just a name with no functional properties. Only typesetting rules are attached.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
QuarkFieldChiDagger
```

$$\chi ^{\dagger }$$

```mathematica
QuantumField[QuarkFieldChiDagger] . GA[\[Mu]] . CovariantD[\[Mu]] . QuantumField[QuarkFieldChi]
ExpandPartialD[%]
```

$$\chi ^{\dagger }.\bar{\gamma }^{\mu }.D_{\mu }.\chi$$

$$\bar{\gamma }^{\mu } \chi ^{\dagger }.\left(\left.(\partial _{\mu }\chi \right)\right)-i T^{\text{c24}} g_s \bar{\gamma }^{\mu } \chi ^{\dagger }.A_{\mu }^{\text{c24}}.\chi$$
