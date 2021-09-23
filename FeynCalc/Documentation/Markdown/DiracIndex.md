## DiracIndex

`DiracIndex` is the head of Dirac indices. The internal representation of a four-dimensional spinorial index `i` is `DiracIndex[i]`.

If the first argument is an integer, `DiracIndex[i]` turns into `ExplicitDiracIndex[i]`.

Dirac indices are the indices that denote the components of Dirac matrices or spinors. They should not be confused with the Lorentz indices attached to the Dirac matrices. For example in the case of $\gamma_{ij}^{\mu}$,  $\mu$ is a Lorentz index, while $i$ and $j$ are Dirac (spinorial) indices.

### See also

[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [ExplicitDiracIndex](ExplicitDiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).

### Examples

```mathematica
DiracIndex[i]
% // StandardForm
```

$$i$$

```
(*DiracIndex[i]*)
```

```mathematica
DiracIndex[2]
% // StandardForm
```

$$2$$

```
(*ExplicitDiracIndex[2]*)
```

```mathematica
DIDelta[i, j] // FCI // StandardForm

(*DiracIndexDelta[DiracIndex[i], DiracIndex[j]]*)
```
