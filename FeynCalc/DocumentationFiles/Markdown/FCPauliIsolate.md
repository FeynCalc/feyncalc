## FCPauliIsolate

`FCPauliIsolate[exp]` wraps chains of Pauli matrices into heads specified by the user.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCPauliIsolate[y SI[i] + x PauliXi[-I] . SIS[p1] . PauliEta[I] . PauliEta[-I] . SIS[p2] . PauliXi[I], Head -> pChain]
```

$$y \;\text{pChain}\left(\bar{\sigma }^i\right)+x \;\text{pChain}\left(\xi ^{\dagger }.\left(\bar{\sigma }\cdot \overline{\text{p1}}\right).\eta \right) \;\text{pChain}\left(\eta ^{\dagger }.\left(\bar{\sigma }\cdot \overline{\text{p2}}\right).\xi \right)$$
