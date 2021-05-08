##  DiracIndexDelta 

DiracIndexDelta[DiracIndex[i], DiracIndex[j]] is the Kronecker-delta in the Dirac space with two explicit Dirac indices i and j..

###  See also 

DiracChain, DCHN, DiracIndex, DIDelta, DiracChainJoin, DiracChainCombine, DiracChainExpand, DiracChainFactor.

###  Examples 

```mathematica
DiracIndexDelta[DiracIndex[i], DiracIndex[j]] 
 
DiracIndexDelta[DiracIndex[i], DiracIndex[j]]^2 
 
DiracChainJoin[%] 
 
DiracChainJoin[%%, TraceOfOne -> D] 
 
DiracIndexDelta[DiracIndex[i], DiracIndex[j]] DiracIndexDelta[DiracIndex[j], DiracIndex[k]] 
 
DiracChainJoin[%] 
 
DiracIndexDelta[DiracIndex[i2], DiracIndex[i3]] DiracIndexDelta[DiracIndex[i4], DiracIndex[i5]] DiracChain[DiracIndex[i7], Spinor[-Momentum[q], 0, 1]] DiracChain[Spinor[Momentum[p], m, 1], DiracIndex[i0]] DiracChain[DiracGamma[LorentzIndex[\[Mu]]], DiracIndex[i1], DiracIndex[i2]] DiracChain[DiracGamma[LorentzIndex[\[Nu]]], DiracIndex[i5], DiracIndex[i6]] DiracChain[m + DiracGamma[Momentum[p]], DiracIndex[i3], DiracIndex[i4]] 
 
DiracChainJoin[%] 
 
DiracChainJoin[% DIDelta[i0, i1]] 
 
DiracChainJoin[% DIDelta[i7, i6]]
```

$$\delta _{ij}$$

$$\delta _{ij}^2$$

$$4$$

$$D$$

$$\delta _{ij} \delta _{jk}$$

$$\delta _{ik}$$

$$\delta _{\text{i2}\text{i3}} \delta _{\text{i4}\text{i5}} \left(\bar{\gamma }^{\mu }\right){}_{\text{i1}\text{i2}} \left(\bar{\gamma }^{\nu }\right){}_{\text{i5}\text{i6}} \left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\bar{\gamma }\cdot \overline{p}+m\right)_{\text{i3}\text{i4}} \left(\varphi (\overline{p},m)\right)_{\text{i0}}$$

$$\left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\varphi (\overline{p},m)\right)_{\text{i0}} \left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }\right){}_{\text{i1}\text{i6}}$$

$$\left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\varphi (\overline{p},m).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }\right){}_{\text{i6}}$$

$$\left(\varphi (\overline{p},m)\right).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }.\left(\varphi (-\overline{q})\right)$$