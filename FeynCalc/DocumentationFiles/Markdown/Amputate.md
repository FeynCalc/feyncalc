##  Amputate 

Amputate[exp, q1, q2, ...] amputates Eps and DiracGamma. Amputate[exp,q1,q2,Pair->{p}] amputates also p.q1 and p.q2; Pair->All amputates all except OPEDelta..

###  See also 

DiracGamma, GA, DiracSimplify, GS, DiracTrick.

###  Examples 

```mathematica
GS[p] . GS[q] 
 
Amputate[%, q]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$

$$q^{\text{$\$$AL$\$$15676}(1)} \left(\bar{\gamma }\cdot \overline{p}\right).\gamma ^{\text{$\$$AL$\$$15676}(1)}$$