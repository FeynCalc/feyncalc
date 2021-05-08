##  SPE 

SPE[a, b] denotes a D-4-dimensional scalar product. SPE[a, b] is transformed into Pair[Momentum[a, -4 + D], Momentum[b, -4 + D]] by FeynCalcInternal. SPE[p] is the same as SPE[p,p]  $\left(=p^2\right)$.

###  See also 

PD, Calc, ExpandScalarProduct, ScalarProduct, SPD.

###  Examples 

```mathematica
SPE[p, q] + SPE[q] 
 
SPE[p - q, q + 2 p] 
 
Calc[ SPE[p - q, q + 2 p] ] 
 
ExpandScalarProduct[SPE[p - q]] 
 
SPE[a, b] // StandardForm 
 
SPE[a, b] // FCI // StandardForm 
 
SPE[a, b] // FCI // FCE // StandardForm 
 
FCE[ChangeDimension[SP[p, q], D - 4]] // StandardForm
```

$$\hat{p}\cdot \hat{q}+\hat{q}^2$$

$$(\hat{p}-\hat{q})\cdot (2 \hat{p}+\hat{q})$$

$$-\hat{p}\cdot \hat{q}+2 \hat{p}^2-\hat{q}^2$$

$$-2 \left(\hat{p}\cdot \hat{q}\right)+\hat{p}^2+\hat{q}^2$$

```
(*SPE[a, b]*)

(*Pair[Momentum[a, -4 + D], Momentum[b, -4 + D]]*)

(*SPE[a, b]*)

(*SPE[p, q]*)
```