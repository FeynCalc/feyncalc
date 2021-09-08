## QuarkFieldPsiDagger

`QuarkFieldPsiDagger` is the name of a fermionic field.This is just a name with no functional properties. Only typesetting rules are attached.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
QuarkFieldPsiDagger
```

$$\psi ^{\dagger }$$

```mathematica
QuantumField[QuarkFieldPsiDagger] . GA[\[Mu]] . CovariantD[\[Mu]] . QuantumField[QuarkFieldPsi]
ExpandPartialD[%]
```

$$\psi ^{\dagger }.\bar{\gamma }^{\mu }.D_{\mu }.\psi$$

$$\bar{\gamma }^{\mu } \psi ^{\dagger }.\left(\left.(\partial _{\mu }\psi \right)\right)-i T^{\text{c24}} g_s \bar{\gamma }^{\mu } \psi ^{\dagger }.A_{\mu }^{\text{c24}}.\psi$$
