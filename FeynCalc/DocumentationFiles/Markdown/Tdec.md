`Tdec[{{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}]` calculates the tensorial decomposition formulas for Lorentzian integrals. The more common ones are saved in `TIDL`.

###  See also 

TID, TIDL, OneLoopSimplify.

###  Examples 

Check that $\int d^D f(p,q) q^{\mu}= \frac{p^{\mu}}{p^2} \int d^D f(p,q) p \cdot q$

```mathematica
Tdec[{q, \[Mu]}, {p}]
%[[2]] /. %[[1]]
```

$$\left\{\left\{\text{X1}\to p\cdot q,\text{X2}\to p^2\right\},\frac{\text{X1} p^{\mu }}{\text{X2}}\right\}$$

$$\frac{p^{\mu } (p\cdot q)}{p^2}$$

This calculates integral transformation for any $\int d^D q_1 d^D q_2 d^D q_3$ $f(p,q_1,q_2,q_3) q_1^{\mu} q_2^{\nu}q_3^{\rho}$.

```mathematica
Tdec[{{Subscript[q, 1], \[Mu]}, {Subscript[q, 2], \[Nu]}, {Subscript[q, 3], \[Rho]}}, {p}, List -> False]
Contract[% FVD[p, \[Mu]] FVD[p, \[Nu]] FVD[p, \[Rho]]] // Factor 
  
 

```

$$\frac{p^{\rho } g^{\mu \nu } \left(p\cdot q_3\right) \left(\left(p\cdot q_1\right) \left(p\cdot q_2\right)-p^2 \left(q_1\cdot q_2\right)\right)}{(1-D) p^2^2}+\frac{p^{\nu } g^{\mu \rho } \left(p\cdot q_2\right) \left(\left(p\cdot q_1\right) \left(p\cdot q_3\right)-p^2 \left(q_1\cdot q_3\right)\right)}{(1-D) p^2^2}+\frac{p^{\mu } g^{\nu \rho } \left(p\cdot q_1\right) \left(\left(p\cdot q_2\right) \left(p\cdot q_3\right)-p^2 \left(q_2\cdot q_3\right)\right)}{(1-D) p^2^2}-\frac{p^{\mu } p^{\nu } p^{\rho } \left(D \left(p\cdot q_1\right) \left(p\cdot q_2\right) \left(p\cdot q_3\right)+2 \left(p\cdot q_1\right) \left(p\cdot q_2\right) \left(p\cdot q_3\right)-p^2 \left(q_1\cdot q_2\right) \left(p\cdot q_3\right)-p^2 \left(q_1\cdot q_3\right) \left(p\cdot q_2\right)-p^2 \left(q_2\cdot q_3\right) \left(p\cdot q_1\right)\right)}{(1-D) p^2^3}$$

$$\left(p\cdot q_1\right) \left(p\cdot q_2\right) \left(p\cdot q_3\right)$$