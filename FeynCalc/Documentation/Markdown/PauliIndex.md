## PauliIndex

`PauliIndex` is the head of Pauli indices. The internal representation of a two-dimensional spinorial index `i` is `PauliIndex[i]`.

If the first argument is an integer, `PauliIndex[i]` turns into `ExplicitPauliIndex[i]`.

Pauli indices are the indices that denote the components of Pauli matrices or spinors. They should not be confused with the Cartesian indices attached to the Pauli matrices. For example in the case of $\sigma_{ij}^{k}$,  $k$ is a Lorentz index, while $i$ and $j$ are Pauli (spinorial) indices.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [ExplicitPauliIndex](ExplicitPauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [DIDelta](DIDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainCombine](PauliChainCombine.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

```mathematica
PauliIndex[i]
% // StandardForm
```

$$i$$

```
(*PauliIndex[i]*)
```

```mathematica
PauliIndex[2]
% // StandardForm
```

$$2$$

```
(*ExplicitPauliIndex[2]*)
```

```mathematica
PIDelta[i, j] // FCI // StandardForm

(*PauliIndexDelta[PauliIndex[i], PauliIndex[j]]*)
```
