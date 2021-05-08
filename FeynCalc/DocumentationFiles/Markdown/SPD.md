##  SPD 

SPD[a, b] denotes a D-dimensional scalar product. SPD[a, b] is transformed into ScalarProduct[a, b,Dimension->D] by FeynCalcInternal. SPD[p] is the same as SPD[p,p] $\left(=p^2\right.$)..

###  See also 

PD, Calc, ExpandScalarProduct, ScalarProduct.

###  Examples 

```mathematica
SPD[p, q] + SPD[q] 
 
SPD[p - q, q + 2 p] 
 
Calc[ SPD[p - q, q + 2 p] ] 
 
ExpandScalarProduct[SPD[p - q]] 
 
SPD[a, b] // StandardForm 
 
SPD[a, b] // FCI // StandardForm 
 
SPD[a, b] // FCI // FCE // StandardForm 
 
FCE[ChangeDimension[SP[p, q], D]] // StandardForm
```

$$p\cdot q+q^2$$

$$(p-q)\cdot (2 p+q)$$

$$-p\cdot q+2 p^2-q^2$$

$$-2 (p\cdot q)+p^2+q^2$$

```
(*SPD[a, b]*)

(*Pair[Momentum[a, D], Momentum[b, D]]*)

(*SPD[a, b]*)

(*SPD[p, q]*)
```