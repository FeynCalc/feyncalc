## DiracChainFactor

`DiracChainFactor[exp]` factors out all expressions inside a `DiracChain` to which the chain doesn't apply. For example, all objects that are not Dirac matrices can be safely factrored out from every Dirac chain.

### See also

[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md).

### Examples

```mathematica
DCHN[FV[p, \[Nu]] GA[\[Mu]] . GA[\[Nu]] . GA[\[Mu]], i, j] 
DiracChainFactor[%]
```

$$\left(\overline{p}^{\nu } \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu }\right){}_{ij}$$

$$\overline{p}^{\nu } \left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu }\right){}_{ij}$$
