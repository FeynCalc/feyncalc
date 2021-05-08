##  PauliEta 

PauliEta[I] represents a two-component Pauli spinor η, while PauliEta[-I] stands for $\eta ^{\dagger }$.

###  Examples 

```mathematica
PauliEta[I] 
 
PauliEta[-I] 
 
PauliEta[-I] . SIS[p] . PauliXi[I] 
 
% // ComplexConjugate
```

$$\eta$$

$$\eta ^{\dagger }$$

$$\eta ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\xi$$

$$\xi ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\eta$$