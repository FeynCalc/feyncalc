##  PauliXi 

PauliXi[I] represents a two-component Pauli spinor ξ, while PauliXi[-I] stands for $\xi ^{\dagger }$.

###  Examples 

```mathematica
PauliXi[I] 
 
PauliXi[-I] 
 
PauliXi[-I] . SIS[p] . PauliEta[I] 
 
% // ComplexConjugate
```

$$\xi$$

$$\xi ^{\dagger }$$

$$\xi ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\eta$$

$$\eta ^{\dagger }.\left(\bar{\sigma }\cdot \overline{p}\right).\xi$$