##  DiracSigma 

DiracSigma[a, b] stands for i/2*(a . b - b . a) in 4 dimensions. a and b must have Head DiracGamma, GA or GS. Only antisymmetry is implemented..

###  See also 

DiracSigmaExplicit.

###  Examples 

```mathematica
DiracSigma[GA[\[Alpha]], GA[\[Beta]]] 
 
DiracSigmaExplicit[%] 
 
DiracSigma[GA[\[Beta]], GA[\[Alpha]]] 
 
DiracSigma[GS[p], GS[q]] 
 
DiracSigmaExplicit[%]
```

$$\sigma ^{\alpha \beta }$$

$$\frac{1}{2} i \left(\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }-\bar{\gamma }^{\beta }.\bar{\gamma }^{\alpha }\right)$$

$$-\sigma ^{\alpha \beta }$$

$$\sigma ^{pq}$$

$$\frac{1}{2} i \left(\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)-\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}\right)\right)$$

The antisymmetry propery is built-in

```mathematica
DiracSigma[GA[\[Alpha]], GA[\[Alpha]]]
```

$$0$$