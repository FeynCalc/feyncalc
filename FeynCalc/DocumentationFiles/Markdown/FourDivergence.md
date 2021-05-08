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

$$![1h0u8wjprk3to](img/1h0u8wjprk3to.png)$$

$$![0qmg0qberoqzp](img/0qmg0qberoqzp.png)$$