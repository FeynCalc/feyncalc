##  CSPD 

CSPD[p, q] is the D-1-dimensional scalar product of p with q and is transformed into CartesianPair[CartesianMomentum[p, D-1],CartesianMomentum[q, D-1]] by FeynCalcInternal. CSPD[p] is the same as CSPD[p,p] ($=p^2$)..

###  See also 

SPD, ScalarProduct, CartesianScalarProduct.

###  Examples 

```mathematica
CSPD[p, q] + CSPD[q] 
 
CSPD[p - q, q + 2 p] 
 
Calc[ CSPD[p - q, q + 2 p] ] 
 
ExpandScalarProduct[CSPD[p - q]] 
 
CSPD[a, b] // StandardForm 
 
CSPD[a, b] // FCI // StandardForm 
 
CSPD[a, b] // FCI // FCE // StandardForm 
 
FCE[ChangeDimension[CSP[p, q], D]] // StandardForm
```

$$p\cdot q+q^2$$

$$(p-q)\cdot (2 p+q)$$

$$-p\cdot q+2 p^2-q^2$$

$$-2 (p\cdot q)+p^2+q^2$$

```
(*CSPD[a, b]*)

(*CartesianPair[CartesianMomentum[a, -1 + D], CartesianMomentum[b, -1 + D]]*)

(*CSPD[a, b]*)

(*CSPD[p, q]*)
```