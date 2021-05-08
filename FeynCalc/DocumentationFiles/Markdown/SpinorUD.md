##  SpinorUD 

SpinorUD[p, m] denotes a $u(p,m)$-spinor that depends on the $\text{D}$-dimensional momentum $\text{p}$..

###  See also 

Spinor, SpinorUBar, SpinorU, SpinorV, SpinorVBar, SpinorUBarD, SpinorVD, SpinorVBarD.

###  Examples 

```mathematica
SpinorUD[p, m] 
 
FCI[%] // StandardForm 
 
SpinorUD[p] 
 
FCI[%] // StandardForm 
 
GSD[p] . SpinorUD[p] 
 
DiracEquation[%]
```

$$u(p,m)$$

```
(*Spinor[Momentum[p, D], m, 1]*)
```

$$u(p)$$

```
(*Spinor[Momentum[p, D], 0, 1]*)
```

$$(\gamma \cdot p).u(p)$$

$$0$$