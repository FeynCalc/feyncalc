##  SP 

SP[a, b] denotes a four-dimensional scalar product. SP[a, b] is transformed into ScalarProduct[a, b] by FeynCalcInternal. SP[p] is the same as SP[p, p] $\left(=p^2\right.$)..

###  See also 

Calc, ExpandScalarProduct, ScalarProduct.

###  Examples 

```mathematica
SP[p, q] + SP[q] 
 
SP[p - q, q + 2 p] 
 
Calc[ SP[p - q, q + 2 p] ] 
 
ExpandScalarProduct[SP[p - q]] 
 
SP[a, b] // StandardForm 
 
SP[a, b] // FCI // StandardForm 
 
SP[a, b] // FCI // FCE // StandardForm
```

$$\overline{p}\cdot \overline{q}+\overline{q}^2$$

$$(\overline{p}-\overline{q})\cdot (2 \overline{p}+\overline{q})$$

$$-\overline{p}\cdot \overline{q}+2 \overline{p}^2-\overline{q}^2$$

$$-2 \left(\overline{p}\cdot \overline{q}\right)+\overline{p}^2+\overline{q}^2$$

```
(*SP[a, b]*)

(*Pair[Momentum[a], Momentum[b]]*)

(*SP[a, b]*)
```