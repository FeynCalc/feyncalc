## ExplicitDiracIndex

`ExplicitDiracIndex[ind]` is an explicit Dirac index, i.e., `ind` is an integer.

### See also

[DiracChain](DiracChain), [DCHN](DCHN), [DiracIndex](DiracIndex), [DiracIndexDelta](DiracIndexDelta), [DIDelta](DIDelta), [DiracChainJoin](DiracChainJoin), [DiracChainCombine](DiracChainCombine), [DiracChainExpand](DiracChainExpand), [DiracChainFactor](DiracChainFactor).

### Examples

```mathematica
DCHN[GA[\[Mu]], 1, 2]
% // FCI // StandardForm
```

$$\left(\bar{\gamma }^{\mu }\right){}_{12}$$

```
(*DiracChain[DiracGamma[LorentzIndex[\[Mu]]], ExplicitDiracIndex[1], ExplicitDiracIndex[2]]*)
```