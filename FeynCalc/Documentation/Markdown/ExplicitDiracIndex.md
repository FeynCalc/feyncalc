## ExplicitDiracIndex

`ExplicitDiracIndex[ind]` is an explicit Dirac index, i.e., `ind` is an integer.

### See also

[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).

### Examples

```mathematica
DCHN[GA[\[Mu]], 1, 2]
% // FCI // StandardForm
```

$$\left(\bar{\gamma }^{\mu }\right){}_{12}$$

```
(*DiracChain[DiracGamma[LorentzIndex[\[Mu]]], ExplicitDiracIndex[1], ExplicitDiracIndex[2]]*)
```
