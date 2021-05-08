##  ExplicitDiracIndex 

ExplicitDiracIndex[ind] is an explicit Dirac index, i.e., $\text{ind}$ is an integer.

###  See also 

DiracChain, DCHN, DiracIndex, DiracIndexDelta, DIDelta, DiracChainJoin, DiracChainCombine, DiracChainExpand, DiracChainFactor.

###  Examples 

```mathematica
DCHN[GA[\[Mu]], 1, 2] 
 
% // FCI // StandardForm
```

$$\left(\bar{\gamma }^{\mu }\right){}_{12}$$

```
(*DiracChain[DiracGamma[LorentzIndex[\[Mu]]], ExplicitDiracIndex[1], ExplicitDiracIndex[2]]*)
```