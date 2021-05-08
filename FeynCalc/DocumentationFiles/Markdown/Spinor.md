##  Spinor 

Spinor[p, m, o] is the head of Dirac spinors. Which of the spinors u, v, $bar{u}$ or $overset{_ }{v}$is understood, depends on the sign of the momentum (p) argument and the relative position in the chain.Spinor[Momentum[p],m] means $bar{u}$ if it stands at the beginning of the chain.Spinor[Momentum[p],m] means $text{u}$ if it stands at the end of the chain.Spinor[-Momentum[p],m] means $bar{v}$ if it stands at the beginning of the chain.Spinor[-Momentum[p],m] means $text{v}$ if it stands at the end of the chain.Spinors of fermions of mass m are normalized to have square $bar{u}$u=2 m and  $overset{_ }{v}$v=-2 m.The optional argument o can be used for additional degrees of freedom. If no optional argument o is supplied, a 1 is subsituted in..

###  See also 

FermionSpinSum, DiracSimplify, SpinorU, SpinorV, SpinorUBar, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.

###  Examples 

```mathematica
Spinor[Momentum[p]] 
 
Spinor[Momentum[p], m]
```

$$\varphi (\overline{p})$$

$$\varphi (\overline{p},m)$$

FeynCalc uses covariant normalization (as opposed to e.g. the normalization used in Bjorken&Drell).

```mathematica
Spinor[Momentum[p], m] . Spinor[Momentum[p], m] // DiracSimplify 
 
DiracSimplify[Spinor[-Momentum[p], m] . GS[p]] 
 
Spinor[Momentum[p]] // StandardForm 
 
ChangeDimension[Spinor[Momentum[p]], D] // StandardForm 
 
Spinor[Momentum[p], m] // StandardForm
```

$$2 m$$

$$-m \left(\varphi (-\overline{p},m)\right)$$

```
(*Spinor[Momentum[p], 0, 1]*)

(*Spinor[Momentum[p, D], 0, 1]*)

(*Spinor[Momentum[p], m, 1]*)
```

SmallVariable's are discarded by Spinor.

```mathematica
Spinor[Momentum[p], SmallVariable[m]] // StandardForm

(*Spinor[Momentum[p], 0, 1]*)
```