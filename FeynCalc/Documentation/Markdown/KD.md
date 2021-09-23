## KD

`KD[i, j]`  is the Kronecker delta in $3$ dimensions.

### See also

[Overview](Extra/FeynCalc.md), [CartesianPair](CartesianPair.md), [KDD](KDD.md).

### Examples

```mathematica
KD[i, j]
```

$$\bar{\delta }^{ij}$$

```mathematica
Contract[KD[i, j] KD[i, j]]
```

$$3$$

```mathematica
KD[a, b] // StandardForm

(*KD[a, b]*)
```

```mathematica
FCI[KD[a, b]] // StandardForm

(*CartesianPair[CartesianIndex[a], CartesianIndex[b]]*)
```

```mathematica
FCE[FCI[KD[a, b]]] // StandardForm

(*KD[a, b]*)
```
