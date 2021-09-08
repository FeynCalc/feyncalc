## PauliXi

`PauliXi[I]` represents a two-component Pauli spinor $\xi$, while `PauliXi[-I]` stands for $\xi^{\dagger }$.

### See also

[Overview](Extra/FeynCalc.md), [PauliEta](PauliEta.md).

### Examples

```mathematica
PauliXi[I]
```

$$\xi$$

```mathematica
PauliXi[-I]
```

$$\xi ^{\dagger }$$

```mathematica
PauliXi[-I] . SIS[p] . PauliEta[I]
% // ComplexConjugate 
  
 

```

$$\xi ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\eta$$

$$\eta ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\xi$$
