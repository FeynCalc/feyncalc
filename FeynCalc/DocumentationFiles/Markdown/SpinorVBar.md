##  SpinorVBar 

SpinorVBar[p, m] denotes a $\bar{v}(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorUBar, SpinorU, SpinorV, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.

###  Examples 

```mathematica
SpinorVBar[p, m] 
 
FCI[%] // StandardForm 
 
SpinorVBar[p] 
 
FCI[%] // StandardForm 
 
SpinorVBar[p] . GS[p] 
 
DiracEquation[%]
```

$$\bar{v}(p,m)$$

```
(*Spinor[-Momentum[p], m, 1]*)
```

$$\bar{v}(p)$$

```
(*Spinor[-Momentum[p], 0, 1]*)
```

$$\bar{v}(p).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$0$$