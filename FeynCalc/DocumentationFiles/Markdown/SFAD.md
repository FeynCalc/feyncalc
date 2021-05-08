##  SFAD 

SFAD[{{q1 +..., p1 . q2 +...,} {m^2, s}, n}, ...] denotes a Lorentzian propagator given by 1/[(q1+...)^2 + p1.q2 ... + m^2 + sign*I*eta]^n, where  q1^2 and p1.q2 are scalar products of Lorentz vectors in D dimensions. For brevity one can also use shorter forms such as SFAD[{q1+ ...,  m^2}, ...], SFAD[{q1+ ...,  m^2 , n}, ...], SFAD[{q1+ ...,  {m^2, -1}}, ...], SFAD[q1,...]  etc. If s is not explicitly specified, then its value is determined by the option EtaSign, which has the default value +1. If n is not explicitly specified, then the default value 1 is assumed. Translation into FeynCalc internal form is performed by FeynCalcInternal, where a SFAD is encoded using the special head StandardPropagatorDenominator..

###  Examples 

```mathematica
SFAD[{{p, 0}, m^2}] 
 
SFAD[{{p, 0}, {m^2, -1}}] 
 
SFAD[{{p, 0}, {-m^2, -1}}] 
 
SFAD[{{0, p . q}, m^2}]
```

$$![1pqbcmp073sbx](img/1pqbcmp073sbx.png)$$

$$![1dwh3g9tt78ij](img/1dwh3g9tt78ij.png)$$

$$![1u58w6ia1brjd](img/1u58w6ia1brjd.png)$$

$$\frac{1}{(p\cdot q-m^2+i \eta )}$$