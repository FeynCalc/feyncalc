##  DIDelta 

DIDelta[i, j] is the Kronecker-delta in the Dirac space. DIDelta[i,j] is transformed into DiracDelta[DiracIndex[i],DiracIndex[j]] by FeynCalcInternal..

###  See also 

DiracChain, DCHN, DiracIndex, DiracIndexDelta, DiracChainJoin, DiracChainExpand, DiracChainFactor.

###  Examples 

```mathematica
DIDelta[i, j] 
 
DIDelta[i, i] 
 
DiracChainJoin[%] 
 
DIDelta[i, j]^2 
 
DiracChainJoin[%] 
 
DIDelta[i, j] DIDelta[j, k] 
 
DiracChainJoin[%] 
 
DCHN[SpinorUBar[p, m], i0] DCHN[GA[\[Mu]], i1, i2] DCHN[GS[p] + m, i3,i4] DCHN[GA[\[Nu]], i5, i6] DIDelta[i2, i3] DIDelta[i4, i5] DCHN[i7, SpinorV[q]] 
 
DCHN[SpinorUBar[p, m], i0] DCHN[GA[\[Mu]], i1, i2] DCHN[GS[p] + m, i3,i4] DCHN[GA[\[Nu]], i5, i6] DIDelta[i2, i3] DIDelta[i4, i5] DCHN[i7, SpinorV[q]] // FCI // StandardForm 
 
DiracChainJoin[%] 
 
DiracChainJoin[% DIDelta[i0, i1]] 
 
DiracChainJoin[% DIDelta[i7, i6]]
```

$$\delta _{ij}$$

$$\delta _{ii}$$

$$4$$

$$\delta _{ij}^2$$

$$4$$

$$\delta _{ij} \delta _{jk}$$

$$\delta _{ik}$$

$$\delta _{\text{i2}\text{i3}} \delta _{\text{i4}\text{i5}} (v(q))_{\text{i7}} \left(\bar{\gamma }^{\mu }\right){}_{\text{i1}\text{i2}} \left(\bar{\gamma }^{\nu }\right){}_{\text{i5}\text{i6}} \left(\bar{u}(p,m)\right)_{\text{i0}} \left(\bar{\gamma }\cdot \overline{p}+m\right)_{\text{i3}\text{i4}}$$

```
(*DiracChain[DiracIndex[i7], Spinor[-Momentum[q], 0, 1]] DiracChain[Spinor[Momentum[p], m, 1], DiracIndex[i0]] DiracChain[DiracGamma[LorentzIndex[\[Mu]]], DiracIndex[i1], DiracIndex[i2]] DiracChain[DiracGamma[LorentzIndex[\[Nu]]], DiracIndex[i5], DiracIndex[i6]] DiracChain[m + DiracGamma[Momentum[p]], DiracIndex[i3], DiracIndex[i4]] DiracIndexDelta[DiracIndex[i2], DiracIndex[i3]] DiracIndexDelta[DiracIndex[i4], DiracIndex[i5]]*)
```

$$\left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\varphi (\overline{p},m)\right)_{\text{i0}} \left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }\right){}_{\text{i1}\text{i6}}$$

$$\left(\varphi (-\overline{q})\right)_{\text{i7}} \left(\varphi (\overline{p},m).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }\right){}_{\text{i6}}$$

$$\left(\varphi (\overline{p},m)\right).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }.\left(\varphi (-\overline{q})\right)$$