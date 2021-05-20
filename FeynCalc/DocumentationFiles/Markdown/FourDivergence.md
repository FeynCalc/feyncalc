##  FourDivergence 

FourDivergence[exp, FV[p, mu]] calculates the partial derivative of exp w.r.t $p^{\mu }$. FourDivergence[exp, FV[p, mu], FV[p,nu], ...] gives the multiple derivative..

###  See also 

RussianTrick.

###  Examples 

```mathematica
SP[p, q] 
 
FourDivergence[%, FV[q, \[Mu]]] 
 
SP[p - k, q] 
 
FourDivergence[%, FV[k, \[Mu]]] 
 
SFAD[{p, m^2}] 
 
FourDivergence[%, FVD[p, \[Nu]]]
```

$$\overline{p}\cdot \overline{q}$$

$$\overline{p}^{\mu }$$

$$(\overline{p}-\overline{k})\cdot \overline{q}$$

$$-\overline{q}^{\mu }$$

$$\frac{1}{(p^2-m^2+i \eta )}$$

$$-\frac{2 p^{\nu }}{(p^2-m^2+i \eta )^2}$$