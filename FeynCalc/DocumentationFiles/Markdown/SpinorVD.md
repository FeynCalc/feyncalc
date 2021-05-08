##  SpinorVD 

SpinorVD[p, m] denotes a $v(p,m)$-spinor that depends on the $\text{D}$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorUBar, SpinorU, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVBarD.

###  Examples 

```mathematica
SpinorVD[p, m] 
 
FCI[%] // StandardForm 
 
SpinorVD[p] 
 
FCI[%] // StandardForm 
 
GSD[p] . SpinorVD[p] 
 
DiracEquation[%]
```

$$v(p,m)$$

```
(*Spinor[-Momentum[p, D], m, 1]*)
```

$$v(p)$$

```
(*Spinor[-Momentum[p, D], 0, 1]*)
```

$$(\gamma \cdot p).v(p)$$

$$0$$