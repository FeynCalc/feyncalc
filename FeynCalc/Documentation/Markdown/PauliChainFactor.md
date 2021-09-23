## PauliChainFactor

`PauliChainFactor[exp]` factors out all expressions inside a `PauliChain` to which the chain doesn't apply. For example, all objects that are not Pauli matrices can be safely factored out from every Pauli chain.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [DIDelta](DIDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainCombine](PauliChainCombine.md), [PauliChainExpand](PauliChainExpand.md).

### Examples

```mathematica
PCHN[CV[p, \[Nu]] CSI[a] . CSI[b] . CSI[a], i, j] 
PauliChainFactor[%]
```

$$\left(\overline{p}^{\nu } \overline{\sigma }^a.\overline{\sigma }^b.\overline{\sigma }^a\right){}_{ij}$$

$$\overline{p}^{\nu } \left(\overline{\sigma }^a.\overline{\sigma }^b.\overline{\sigma }^a\right){}_{ij}$$
