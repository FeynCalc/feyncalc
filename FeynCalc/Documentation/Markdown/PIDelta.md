## PIDelta

`PIDelta[i,j]` is the Kronecker-delta in the Pauli space. `PIDelta[i,j]` is transformed into `PauliIndexDelta[PauliIndex[i],PauliIndex[j]]` by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

```mathematica
PIDelta[i, j]
```

$$\delta _{ij}$$

```mathematica
PIDelta[i, i]
PauliChainJoin[%]
```

$$\delta _{ii}$$

$$4$$

```mathematica
PIDelta[i, j]^2
PauliChainJoin[%]
```

$$\delta _{ij}^2$$

$$4$$

```mathematica
PIDelta[i, j] PIDelta[j, k]
PauliChainJoin[%]
```

$$\delta _{ij} \delta _{jk}$$

$$\delta _{ik}$$

```mathematica
ex = PIDelta[i2, i3] PIDelta[i4, i5]  PCHN[i7, PauliXi[I]] PauliChain[PauliEta[-I], PauliIndex[i0]] PauliChain[PauliSigma[CartesianIndex[a]], PauliIndex[i1], PauliIndex[i2]] PauliChain[PauliSigma[CartesianIndex[b]], PauliIndex[i5], PauliIndex[i6]] PauliChain[m + PauliSigma[CartesianMomentum[p]], PauliIndex[i3], PauliIndex[i4]]
```

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }\right){}_{\text{i0}} \delta _{\text{i2}\;\text{i3}} \delta _{\text{i4}\;\text{i5}} \left(\overline{\sigma }^a\right){}_{\text{i1}\;\text{i2}} \left(\overline{\sigma }^b\right){}_{\text{i5}\;\text{i6}} \left(\overline{\sigma }\cdot \overline{p}+m\right)_{\text{i3}\;\text{i4}}$$

```mathematica
PauliChainJoin[ex]
```

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }\right){}_{\text{i0}} \left(\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_{\text{i1}\;\text{i6}}$$

```mathematica
PauliChainJoin[ex PIDelta[i0, i1]]
```

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_{\text{i6}}$$

```mathematica
PauliChainJoin[% PIDelta[i7, i6]]
```

$$\eta ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b.\xi$$
