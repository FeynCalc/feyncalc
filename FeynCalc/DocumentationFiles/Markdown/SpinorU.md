##  SpinorU 

SpinorU[p, m] denotes a $u(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorUBar, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.

###  Examples 

```mathematica
SpinorU[p, m] 
 
FCI[%] // StandardForm 
 
SpinorU[p] 
 
FCI[%] // StandardForm 
 
GS[p] . SpinorU[p] 
 
DiracEquation[%]
```

$$u(p,m)$$

```
(*Spinor[Momentum[p], m, 1]*)
```

$$u(p)$$

```
(*Spinor[Momentum[p], 0, 1]*)
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).u(p)$$

$$0$$