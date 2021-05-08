##  PIDelta 

PIDelta[] PIDelta.

###  See also 

PauliChain, PCHN, PauliIndex, PauliIndexDelta, PauliChainJoin, PauliChainExpand, PauliChainFactor.

###  Examples 

```mathematica
PIDelta[i, j] 
 
PIDelta[i, i] 
 
PauliChainJoin[%] 
 
PIDelta[i, j]^2 
 
PauliChainJoin[%] 
 
PIDelta[i, j] PIDelta[j, k] 
 
PauliChainJoin[%] 
 
PIDelta[i2, i3] PIDelta[i4, i5]  PCHN[i7, PauliXi[I]] PauliChain[PauliEta[-I], PauliIndex[i0]] PauliChain[PauliSigma[CartesianIndex[a]], PauliIndex[i1], PauliIndex[i2]] PauliChain[PauliSigma[CartesianIndex[b]], PauliIndex[i5], PauliIndex[i6]] PauliChain[m + PauliSigma[CartesianMomentum[p]], PauliIndex[i3], PauliIndex[i4]] 
 
PauliChainJoin[%] 
 
PauliChainJoin[% PIDelta[i0, i1]] 
 
PauliChainJoin[% PIDelta[i7, i6]]
```

$$\delta _{ij}$$

$$\delta _{ii}$$

$$4$$

$$\delta _{ij}^2$$

$$4$$

$$\delta _{ij} \delta _{jk}$$

$$\delta _{ik}$$

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }\right){}_{\text{i0}} \delta _{\text{i2}\text{i3}} \delta _{\text{i4}\text{i5}} \left(\overline{\sigma }^a\right){}_{\text{i1}\text{i2}} \left(\overline{\sigma }^b\right){}_{\text{i5}\text{i6}} \left(\overline{\sigma }\cdot \overline{p}+m\right)_{\text{i3}\text{i4}}$$

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }\right){}_{\text{i0}} \left(\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_{\text{i1}\text{i6}}$$

$$(\xi )_{\text{i7}} \left(\eta ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_{\text{i6}}$$

$$\eta ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b.\xi$$