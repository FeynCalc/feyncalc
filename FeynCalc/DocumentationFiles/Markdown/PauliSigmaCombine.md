## PauliSigmaCombine

`PauliSigmaCombine[exp]`  is (nearly) the inverse operation to PauliSigmaExpand.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigmaExpand](PauliSigmaExpand.md).

### Examples

```mathematica
SIS[p] + SIS[q]
PauliSigmaCombine[%]
```

$$\bar{\sigma }\cdot \overline{p}+\bar{\sigma }\cdot \overline{q}$$

$$\bar{\sigma }\cdot \left(\overline{p}+\overline{q}\right)$$

```mathematica
PauliXi[-I] . (SIS[p1 + p2] + SIS[q]) . PauliEta[I]
PauliSigmaCombine[%]
```

$$\xi ^{\dagger }.\left(\bar{\sigma }\cdot \left(\overline{\text{p1}}+\overline{\text{p2}}\right)+\bar{\sigma }\cdot \overline{q}\right).\eta$$

$$\xi ^{\dagger }.\left(\bar{\sigma }\cdot \left(\overline{\text{p1}}+\overline{\text{p2}}+\overline{q}\right)\right).\eta$$
