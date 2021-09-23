## PauliChainJoin

`PauliChainJoin[exp]` joins chains of Pauli matrices with explicit Pauli indices wrapped with a head `PauliChain`.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [DIDelta](DIDelta.md), [PauliChainCombine](PauliChainCombine.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

```mathematica
PCHN[PauliXi[-I], i] PCHN[CSID[a] . CSID[b], i, j] PCHN[j, PauliEta[I]]
PauliChainJoin[%]
```

$$(\eta )_j \left(\xi ^{\dagger }\right){}_i \left(\sigma ^a.\sigma ^b\right){}_{ij}$$

$$\xi ^{\dagger }.\sigma ^a.\sigma ^b.\eta$$
