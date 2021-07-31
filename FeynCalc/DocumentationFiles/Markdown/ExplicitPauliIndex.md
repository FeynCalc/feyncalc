`ExplicitPauliIndex[ind]` is an explicit Pauli index, i.e., `ind` is an integer.

### See also

[PauliChain](PauliChain), [PCHN](PCHN), [PauliIndex](PauliIndex), [PauliIndexDelta](PauliIndexDelta), [PIDelta](PIDelta), [PauliChainJoin](PauliChainJoin), [PauliChainCombine](PauliChainCombine), [PauliChainExpand](PauliChainExpand), [PauliChainFactor](PauliChainFactor).

### Examples

```mathematica
PCHN[SI[i], 1, 2]
% // FCI // StandardForm 
  
 

```

$$\left(\bar{\sigma }^i\right){}_{12}$$

```
(*PauliChain[PauliSigma[LorentzIndex[i]], ExplicitPauliIndex[1], ExplicitPauliIndex[2]]*)
```