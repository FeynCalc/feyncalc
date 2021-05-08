##  SpinorUBarD 

SpinorUBarD[p, m] denotes a $\bar{u}(p,m)$-spinor that depends on the $\text{D}$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorUBar, SpinorU, SpinorV, SpinorVBar, SpinorUD, SpinorVD, SpinorVBarD.

###  Examples 

```mathematica
SpinorUBarD[p, m] 
 
FCI[%] // StandardForm 
 
SpinorUBarD[p] 
 
FCI[%] // StandardForm 
 
SpinorUBarD[p] . GSD[p] 
 
DiracEquation[%]
```

$$\bar{u}(p,m)$$

```
(*Spinor[Momentum[p, D], m, 1]*)
```

$$\bar{u}(p)$$

```
(*Spinor[Momentum[p, D], 0, 1]*)
```

$$\bar{u}(p).(\gamma \cdot p)$$

$$0$$