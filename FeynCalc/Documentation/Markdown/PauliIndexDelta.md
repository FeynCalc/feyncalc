## PauliIndexDelta

`PauliIndexDelta[PauliIndex[i], PauliIndex[j]]` is the Kronecker-delta in the Pauli space with two explicit Pauli indices `i` and `j`.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [DIDelta](DIDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainCombine](PauliChainCombine.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

```mathematica
PauliIndexDelta[PauliIndex[i], PauliIndex[j]]
```

$$\delta _{ij}$$

```mathematica
PauliIndexDelta[PauliIndex[i], PauliIndex[j]]^2
PauliChainJoin[%]
PauliChainJoin[%%, TraceOfOne -> D]
```

$$\delta _{ij}^2$$

$$4$$

$$D$$

```mathematica
PauliIndexDelta[PauliIndex[i], PauliIndex[j]] PauliIndexDelta[PauliIndex[j], PauliIndex[k]]
PauliChainJoin[%]
```

$$\delta _{ij} \delta _{jk}$$

$$\delta _{ik}$$

```mathematica
PauliChain[PauliEta[-I], PauliIndex[i0]] PIDelta[i0, i1] // FCI // PauliChainJoin
```

$$\left(\eta ^{\dagger }\right){}_{\text{i1}}$$

```mathematica
PauliIndexDelta[PauliIndex[i2], PauliIndex[i3]] PauliIndexDelta[PauliIndex[i4], PauliIndex[i5]] PauliChain[PauliIndex[i7], PauliXi[I]] PauliChain[PauliEta[-I], PauliIndex[i0]] PauliChain[PauliSigma[CartesianIndex[a]], PauliIndex[i1], PauliIndex[i2]] PauliChain[PauliSigma[CartesianIndex[b]], PauliIndex[i5], PauliIndex[i6]] PauliChain[m + PauliSigma[CartesianMomentum[p]], PauliIndex[i3], PauliIndex[i4]]
PauliChainJoin[%]
```

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }\right){}_{\text{i0}} \delta _{\text{i2}\;\text{i3}} \delta _{\text{i4}\;\text{i5}} \left(\overline{\sigma }^a\right){}_{\text{i1}\;\text{i2}} \left(\overline{\sigma }^b\right){}_{\text{i5}\;\text{i6}} \left(\overline{\sigma }\cdot \overline{p}+m\right)_{\text{i3}\;\text{i4}}$$

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }\right){}_{\text{i0}} \left(\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_{\text{i1}\;\text{i6}}$$

```mathematica
PauliChainJoin[% PIDelta[i0, i1]]
```

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_{\text{i6}}$$

```mathematica
PauliChainJoin[% PIDelta[i7, i6]]
```

$$\eta ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b.\xi$$
