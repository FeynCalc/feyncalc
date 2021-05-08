##  SpinorV 

SpinorV[p, m] denotes a $v(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorUBar, SpinorU, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.

###  Examples 

```mathematica
SpinorV[p, m] 
 
FCI[%] // StandardForm 
 
SpinorV[p] 
 
FCI[%] // StandardForm 
 
GS[p] . SpinorV[p] 
 
DiracEquation[%]
```

$$v(p,m)$$

```
(*Spinor[-Momentum[p], m, 1]*)
```

$$v(p)$$

```
(*Spinor[-Momentum[p], 0, 1]*)
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).v(p)$$

$$0$$