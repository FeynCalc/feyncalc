##  CSP 

CSP[p, q] is the 3-dimensional scalar product of p with q and is transformed into CartesianPair[CartesianMomentum[p],CartesianMomentum[q]] by FeynCalcInternal. CSP[p] is the same as CSP[p,p] ($=p^2$)..

###  See also 

SP, ScalarProduct, CartesianScalarProduct.

###  Examples 

```mathematica
CSP[p, q] + CSP[q] 
 
CSP[p - q, q + 2 p] 
 
Calc[ CSP[p - q, q + 2 p] ] 
 
ExpandScalarProduct[CSP[p - q]] 
 
CSP[a, b] // StandardForm 
 
CSP[a, b] // FCI // StandardForm 
 
CSP[a, b] // FCI // FCE // StandardForm
```

$$\overline{p}\cdot \overline{q}+\overline{q}^2$$

$$(\overline{p}-\overline{q})\cdot (2 \overline{p}+\overline{q})$$

$$-\overline{p}\cdot \overline{q}+2 \overline{p}^2-\overline{q}^2$$

$$-2 \left(\overline{p}\cdot \overline{q}\right)+\overline{p}^2+\overline{q}^2$$

```
(*CSP[a, b]*)

(*CartesianPair[CartesianMomentum[a], CartesianMomentum[b]]*)

(*CSP[a, b]*)
```