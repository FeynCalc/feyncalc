## KDD

`KDD[i, j]` is the Kronecker delta in $D-1$ dimensions.

### See also

[Overview](Extra/FeynCalc.md), [CartesianPair](CartesianPair.md), [KD](KD.md).

### Examples

```mathematica
KDD[i, j]
```

$$\delta ^{ij}$$

```mathematica
Contract[KDD[i, j] KDD[i, j]]
```

$$D-1$$

```mathematica
KDD[a, b] // StandardForm

(*KDD[a, b]*)
```

```mathematica
FCI[KDD[a, b]] // StandardForm

(*CartesianPair[CartesianIndex[a, -1 + D], CartesianIndex[b, -1 + D]]*)
```

```mathematica
FCE[FCI[KDD[a, b]]] // StandardForm

(*KDD[a, b]*)
```
