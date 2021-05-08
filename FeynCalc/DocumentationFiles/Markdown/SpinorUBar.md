##  SpinorUBar 

SpinorUBar[p, m] denotes a $\bar{u}(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorU, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.

###  Examples 

```mathematica
SpinorUBar[p, m] 
 
FCI[%] // StandardForm 
 
SpinorUBar[p] 
 
FCI[%] // StandardForm 
 
SpinorUBar[p] . GS[p] 
 
DiracEquation[%]
```

$$\bar{u}(p,m)$$

```
(*Spinor[Momentum[p], m, 1]*)
```

$$\bar{u}(p)$$

```
(*Spinor[Momentum[p], 0, 1]*)
```

$$\bar{u}(p).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$0$$