## PauliChain

`PauliChain[x, i, j]` denotes a chain of Pauli matrices `x`, where the Pauli indices `i` and `j` are explicit.

### See also

[Overview](Extra/FeynCalc.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

A standalone Pauli matrix $\sigma^i_{jk}$

```mathematica
PauliChain[PauliSigma[CartesianIndex[a]], PauliIndex[i], PauliIndex[j]]
```

$$\left(\overline{\sigma }^a\right){}_{ij}$$

A chain of Pauli matrices with open indices

```mathematica
PauliChain[PauliSigma[CartesianIndex[a, D - 1], D - 1] . PauliSigma[CartesianIndex[b, D - 1], D - 1], PauliIndex[i], PauliIndex[j]]
```

$$\left(\sigma ^a.\sigma ^b\right){}_{ij}$$

A `PauliChain` with only two arguments denotes a spinor component

```mathematica
PauliChain[PauliXi[-I], PauliIndex[i]]
```

$$\left(\xi ^{\dagger }\right){}_i$$

```mathematica
PauliChain[PauliEta[-I], PauliIndex[i]]
```

$$\left(\eta ^{\dagger }\right){}_i$$

```mathematica
PauliChain[PauliIndex[i], PauliXi[I]]
```

$$(\xi )_i$$

```mathematica
PauliChain[PauliIndex[i], PauliEta[I]]
```

$$(\eta )_i$$

The chain may also be partially open or closed

```mathematica
PauliChain[PauliSigma[CartesianIndex[a]] . (m + PauliSigma[CartesianMomentum[p]]) . PauliSigma[CartesianIndex[b]], PauliXi[-I], PauliIndex[j]]
```

$$\left(\xi ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_j$$

```mathematica
PauliChain[PauliSigma[CartesianIndex[a]] . (m + PauliSigma[CartesianMomentum[p]]) . PauliSigma[CartesianIndex[b]], PauliIndex[i], PauliXi[I]]
```

$$\left(\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b.\xi \right){}_i$$

```mathematica
PauliChain[PauliSigma[CartesianIndex[a]] . (m + PauliSigma[CartesianMomentum[p]]) . PauliSigma[CartesianIndex[b]], PauliXi[-I], PauliEta[I]]
```

$$\left(\xi ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b.\eta \right)$$

```mathematica
PauliChain[1, PauliXi[-I], PauliEta[I]]
```

$$\left(\xi ^{\dagger }.\eta \right)$$
