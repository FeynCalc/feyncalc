## PauliEta

`PauliEta[I]` represents a two-component Pauli spinor `\eta`, while `PauliEta[-I]` stands for $\eta^{\dagger }$.

### See also

[Overview](Extra/FeynCalc.md), [PauliXi](PauliXi.md).

### Examples

```mathematica
PauliEta[I]
```

$$\eta$$

```mathematica
PauliEta[-I]
```

$$\eta ^{\dagger }$$

```mathematica
PauliEta[-I] . SIS[p] . PauliXi[I]
% // ComplexConjugate
```

$$\eta ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\xi$$

$$\xi ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\eta$$
