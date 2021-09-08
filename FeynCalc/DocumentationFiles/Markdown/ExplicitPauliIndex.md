## ExplicitPauliIndex

`ExplicitPauliIndex[ind]` is an explicit Pauli index, i.e., `ind` is an integer.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [PIDelta](PIDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainCombine](PauliChainCombine.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

```mathematica
PCHN[SI[i], 1, 2]
% // FCI // StandardForm 
  
 

```

$$\left(\bar{\sigma }^i\right){}_{12}$$

```
(*PauliChain[PauliSigma[LorentzIndex[i]], ExplicitPauliIndex[1], ExplicitPauliIndex[2]]*)
```
