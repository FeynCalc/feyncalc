##  CFAD 

CFAD[{{q1 +..., p1 . q2 +...,} {m^2, s}, n}, ...] denotes a Cartesian propagator given by 1/[(q1+...)^2 + p1.q2 ... + m^2 + sign*I*eta]^n, where q1^2 and p1.q2 are Cartesian scalar products in D-1 dimensions. For brevity one can also use shorter forms such as CFAD[{q1+ ...,  m^2}, ...], CFAD[{q1+ ...,  m^2 , n}, ...], CFAD[{q1+ ...,  {m^2, -1}}, ...], CFAD[q1,...]  etc. If s is not explicitly specified, then its value is determined by the option EtaSign, which has the default value +1. If n is not explicitly specified, then the default value 1 is assumed. Translation into FeynCalc internal form is performed by FeynCalcInternal, where a CFAD is encoded using the special head CartesianPropagatorDenominator..

###  See also 

FAD, SFAD, GFAD, FeynAmpDenominator.

###  Examples 

```mathematica
CFAD[{{p, 0}, m^2}] 
 
FeynAmpDenominatorExplicit[%] 
 
CFAD[{{p, 0}, {m^2, 1}}] 
 
FeynAmpDenominatorExplicit[%] 
 
CFAD[{{p, 0}, -m^2}] 
 
FeynAmpDenominatorExplicit[%] 
 
CFAD[{{0, p . q}, m^2}] 
 
FeynAmpDenominatorExplicit[%] 
 
CFAD[{{0, p . q}}] 
 
FeynAmpDenominatorExplicit[%]
```

$$![0q8mzf7tksbzy](img/0q8mzf7tksbzy.png)$$

$$\frac{1}{m^2+p^2}$$

$$![14y7gjpybrjer](img/14y7gjpybrjer.png)$$

$$\frac{1}{m^2+p^2}$$

$$![1edqpkh85llb3](img/1edqpkh85llb3.png)$$

$$\frac{1}{p^2-m^2}$$

$$\frac{1}{(p\cdot q+m^2-i \eta )}$$

$$\frac{1}{m^2+p\cdot q}$$

$$\frac{1}{(p\cdot q-i \eta )}$$

$$\frac{1}{p\cdot q}$$