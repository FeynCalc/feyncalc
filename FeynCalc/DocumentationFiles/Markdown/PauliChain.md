##  PauliChain 

PauliChain[x, i, j] denotes a chain of Pauli matrices x, where the Pauli indices i and j are explicit..

###  See also 

PCHN, PauliIndex, PauliIndexDelta, PauliChainJoin, PauliChainExpand, PauliChainFactor.

###  Examples 

A standalone Pauli matrix

```mathematica
PauliChain[PauliSigma[CartesianIndex[a]], PauliIndex[i], PauliIndex[j]]
```

$$\left(\overline{\sigma }^a\right){}_{ij}$$

A chain of Pauli matrices with open indices

```mathematica
PauliChain[PauliSigma[CartesianIndex[a, D - 1], D - 1] . PauliSigma[CartesianIndex[b, D - 1], D - 1], PauliIndex[i], PauliIndex[j]]
```

$$\left(\sigma ^a.\sigma ^b\right){}_{ij}$$

A PauliChain with only two arguments denotes a spinor component

```mathematica
PauliChain[PauliXi[-I], PauliIndex[i]] 
 
PauliChain[PauliEta[-I], PauliIndex[i]] 
 
PauliChain[PauliIndex[i], PauliXi[I]] 
 
PauliChain[PauliIndex[i], PauliEta[I]]
```

$$\left(\xi ^{\dagger }\right){}_i$$

$$\left(\eta ^{\dagger }\right){}_i$$

$$(\xi )_i$$

$$(\eta )_i$$

The chain may also be partially open or closed

```mathematica
PauliChain[PauliSigma[CartesianIndex[a]] . (m + PauliSigma[CartesianMomentum[p]]) . PauliSigma[CartesianIndex[b]], PauliXi[-I], PauliIndex[j]] 
 
PauliChain[PauliSigma[CartesianIndex[a]] . (m + PauliSigma[CartesianMomentum[p]]) . PauliSigma[CartesianIndex[b]], PauliIndex[i], PauliXi[I]] 
 
PauliChain[PauliSigma[CartesianIndex[a]] . (m + PauliSigma[CartesianMomentum[p]]) . PauliSigma[CartesianIndex[b]], PauliXi[-I], PauliEta[I]] 
 
PauliChain[1, PauliXi[-I], PauliEta[I]]
```

$$\left(\xi ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b\right){}_j$$

$$\left(\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b.\xi \right){}_i$$

$$\left(\xi ^{\dagger }.\overline{\sigma }^a.\left(\overline{\sigma }\cdot \overline{p}+m\right).\overline{\sigma }^b.\eta \right)$$

$$\left(\xi ^{\dagger }.\eta \right)$$