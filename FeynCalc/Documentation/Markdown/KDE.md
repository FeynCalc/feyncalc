## KDE

`KDE[i, j]`  is the Kronecker delta in $D-4$ dimensions.

### See also

[Overview](Extra/FeynCalc.md), [CartesianPair](CartesianPair.md), [KD](KD.md), [KDD](KDD.md).

### Examples

```mathematica
KDE[i, j]
```

$$\hat{\delta }^{ij}$$

```mathematica
Contract[KDE[i, j] KDE[i, j]]
```

$$D-4$$

```mathematica
Contract[KDE[i, j] KD[i, j]]
```

$$0$$

```mathematica
Contract[KDE[i, j] KDD[i, j]]
```

$$D-4$$

```mathematica
KDE[i, j] // StandardForm

(*KDE[i, j]*)
```

```mathematica
FCI[KDE[i, j]] // StandardForm

(*CartesianPair[CartesianIndex[i, -4 + D], CartesianIndex[j, -4 + D]]*)
```

```mathematica
FCE[FCI[KDE[i, j]]] // StandardForm

(*KDE[i, j]*)
```
