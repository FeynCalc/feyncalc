##  SpinorVBarD 

SpinorVBarD[p, m] denotes a $\bar{v}(p,m)$-spinor that depends on the $\text{D}$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorUBar, SpinorU, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD.

###  Examples 

```mathematica
SpinorVBarD[p, m] 
 
FCI[%] // StandardForm 
 
SpinorVBarD[p] 
 
FCI[%] // StandardForm 
 
SpinorVBarD[p] . GSD[p] 
 
DiracEquation[%]
```

$$\bar{v}(p,m)$$

```
(*Spinor[-Momentum[p, D], m, 1]*)
```

$$\bar{v}(p)$$

```
(*Spinor[-Momentum[p, D], 0, 1]*)
```

$$\bar{v}(p).(\gamma \cdot p)$$

$$0$$