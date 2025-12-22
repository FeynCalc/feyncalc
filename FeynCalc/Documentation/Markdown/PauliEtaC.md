## PauliEtaC

`PauliEta[I]` represents a two-component Pauli spinor $\eta_C$, while `PauliEtaC[-I]` stands for $\eta_C^{\dagger }$.

### See also

[Overview](Extra/FeynCalc.md), [PauliXi](PauliXi.md).

### Examples

```mathematica
PauliEtaC[I]
```

$$\eta _c$$

```mathematica
PauliEtaC[-I]
```

$$\eta _c^{\dagger }$$

```mathematica
PauliEtaC[-I] . SIS[p] . PauliXi[I] 
 
% // ComplexConjugate

```mathematica

$$\eta _c^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\xi$$

$$\xi ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\eta _c$$