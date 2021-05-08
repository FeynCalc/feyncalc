##  ThreeDivergence 

ThreeDivergence[exp, CV[p, i]]  calculates the partial derivative of exp w.r.t. $p^i$. ThreeDivergence[exp, CV[p, i], CV[p,i], ...] gives the multiple derivative..

###  Examples 

```mathematica
CSP[p, q] 
 
ThreeDivergence[%, CV[q, i]] 
 
CSP[p - k, q] 
 
ThreeDivergence[%, CV[k, i]] 
 
CFAD[{p, m^2}] 
 
ThreeDivergence[%, CVD[p, i]]
```

$$\overline{p}\cdot \overline{q}$$

$$\overline{p}^i$$

$$(\overline{p}-\overline{k})\cdot \overline{q}$$

$$-\overline{q}^i$$

$$![0iknn8s8n51dh](img/0iknn8s8n51dh.png)$$

$$![1trnnounno7g1](img/1trnnounno7g1.png)$$