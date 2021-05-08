##  PartialFourVector 

PartialFourVector[exp, FourVector[p, mu]]  calculates the partial derivative of exp w.r.t. p(mu). PartialFourVector[exp, FV[p, mu], FV[p,nu], ...]  gives the multiple derivative..

###  See also 

Pair, Contract.

###  Examples 

```mathematica
PartialFourVector[a E^FV[p, \[Nu]]^2, FV[p, \[Mu]]]
```

$$2 a e^{\overline{p}^2} \overline{p}^{\mu }$$