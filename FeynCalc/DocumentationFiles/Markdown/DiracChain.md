##  DiracChain 

DiracChain[x, i, j] denotes a chain of Dirac matrices x, where the Dirac indices i and j are explicit..

###  See also 

DiracChain, DCHN, DiracIndex, DiracIndexDelta, DiracChainJoin, DiracChainExpand, DiracChainFactor.

###  Examples 

A standalone Dirac matrix

```mathematica
DiracChain[DiracGamma[LorentzIndex[\[Mu]]], DiracIndex[i], DiracIndex[j]]
```

$$\left(\bar{\gamma }^{\mu }\right){}_{ij}$$

A chain of Dirac matrices with open indices

```mathematica
DiracChain[DiracGamma[LorentzIndex[\[Mu], D], D] . DiracGamma[LorentzIndex[\[Nu], D], D], DiracIndex[i], DiracIndex[j]]
```

$$\left(\gamma ^{\mu }.\gamma ^{\nu }\right){}_{ij}$$

A DiracChain with only two arguments denotes a spinor component

```mathematica
DiracChain[Spinor[Momentum[p], m], DiracIndex[i]] 
 
DiracChain[Spinor[Momentum[-p], m], DiracIndex[i]] 
 
DiracChain[DiracIndex[i], Spinor[Momentum[p], m]] 
 
DiracChain[DiracIndex[i], Spinor[Momentum[-p], m]]
```

$$\left(\varphi (\overline{p},m)\right)_i$$

$$\left(\varphi (-\overline{p},m)\right)_i$$

$$\left(\varphi (\overline{p},m)\right)_i$$

$$\left(\varphi (-\overline{p},m)\right)_i$$

The chain may also be partially open or closed

```mathematica
DiracChain[DiracGamma[LorentzIndex[\[Mu]]] . (m + DiracGamma[Momentum[p]]) . DiracGamma[LorentzIndex[\[Nu]]], Spinor[Momentum[p], m, 1], DiracIndex[j]] 
 
DiracChain[DiracGamma[LorentzIndex[\[Mu]]] . (m + DiracGamma[Momentum[p]]) . DiracGamma[LorentzIndex[\[Nu]]], DiracIndex[i], Spinor[Momentum[p], m, 1]] 
 
DiracChain[DiracGamma[LorentzIndex[\[Mu]]] . (m + DiracGamma[Momentum[p]]) . DiracGamma[LorentzIndex[\[Nu]]], Spinor[Momentum[p1], m1, 1], Spinor[Momentum[p2], m2, 1]] 
 
DiracChain[1, Spinor[Momentum[p1], m1, 1], Spinor[Momentum[p2], m2, 1]]
```

$$\left(\varphi (\overline{p},m).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }\right){}_j$$

$$\left(\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }.\varphi (\overline{p},m)\right){}_i$$

$$\left(\varphi (\overline{\text{p1}},\text{m1}).\bar{\gamma }^{\mu }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\nu }.\varphi (\overline{\text{p2}},\text{m2})\right)$$

$$\left(\varphi (\overline{\text{p1}},\text{m1}).\varphi (\overline{\text{p2}},\text{m2})\right)$$