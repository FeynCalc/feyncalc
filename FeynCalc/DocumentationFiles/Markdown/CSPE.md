##  CSPE 

CSPE[p, q] is the D-4-dimensional scalar product of p with q and is transformed into CartesianPair[CartesianMomentum[p, D-4],CartesianMomentum[q, D-4]] by FeynCalcInternal. CSPE[p] is the same as CSPE[p,p] ( $=p^2$)..

###  See also 

SPE, ScalarProduct, CartesianScalarProduct.

###  Examples 

```mathematica
CSPE[p, q] + CSPE[q] 
 
CSPE[p - q, q + 2 p] 
 
Calc[ CSPE[p - q, q + 2 p] ] 
 
ExpandScalarProduct[CSPE[p - q]] 
 
CSPE[a, b] // StandardForm 
 
CSPE[a, b] // FCI // StandardForm 
 
CSPE[a, b] // FCI // FCE // StandardForm 
 
FCE[ChangeDimension[CSP[p, q], D - 4]] // StandardForm
```

$$\hat{p}\cdot \hat{q}+\hat{q}^2$$

$$(\hat{p}-\hat{q})\cdot (2 \hat{p}+\hat{q})$$

$$-\hat{p}\cdot \hat{q}+2 \hat{p}^2-\hat{q}^2$$

$$-2 \left(\hat{p}\cdot \hat{q}\right)+\hat{p}^2+\hat{q}^2$$

```
(*CSPE[a, b]*)

(*CartesianPair[CartesianMomentum[a, -4 + D], CartesianMomentum[b, -4 + D]]*)

(*CSPE[a, b]*)

(*CSPE[p, q]*)
```