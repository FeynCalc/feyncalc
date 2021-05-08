##  FourLaplacian 

FourLaplacian[exp, p, q] is $\partial \left/\partial p_{\mu }\right.$$\partial \left/\partial q_{\mu }\right.$exp..

###  See also 

FourDivergence, RussianTrick.

###  Examples 

```mathematica
SP[q, q] 
 
FourLaplacian[%, q, q] 
 
SOD[q]^OPEmFAD[q, q - p] // FCI 
 
FourLaplacian[%, q, q]
```

$$\overline{q}^2$$

$$2 D$$

$$(\Delta \cdot q)^{\text{OPEmFAD}(q,q-p)}$$

$$0$$