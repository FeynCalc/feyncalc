##  CartesianScalarProduct 

CartesianScalarProduct[p, q]  is the input for the scalar product of two Cartesian vectors p and q. CartesianScalarProduct[p] is equivalent to CartesianScalarProduct[p, p]. Expansion of sums of momenta in CartesianScalarProduct is done with ExpandScalarProduct. Scalar products may be set, e.g. via ScalarProduct[a, b] = m^2; but a and b may not contain sums. Note that ScalarProduct[a, b] = m^2 actually sets Cartesian scalar products in different dimensions specified by the value of the SetDimensions option. It is highly recommended to set ScalarProduct's before any calculation. This improves the performance of FeynCalc..

###  See also 

CSP, CSPD, CSPE.

###  Examples 

```mathematica
CartesianScalarProduct[p, q] 
 
CartesianScalarProduct[p + q, -q] 
 
CartesianScalarProduct[p, p] 
 
CartesianScalarProduct[q] 
 
CartesianScalarProduct[p, q] // StandardForm 
 
CartesianScalarProduct[p, q, Dimension -> D - 1] // StandardForm 
 
CartesianScalarProduct[Subscript[p, 1], Subscript[p, 2]] = s/2 
 
ExpandScalarProduct[ CartesianScalarProduct[Subscript[p, 1] - q, Subscript[p, 2] - k]] 
 
Calc[ CartesianScalarProduct[Subscript[p, 1] - q, Subscript[p, 2] - k]] 
 
FCClearScalarProducts[]
```

$$\overline{p}\cdot \overline{q}$$

$$-\left(\overline{q}\cdot (\overline{p}+\overline{q})\right)$$

$$\overline{p}^2$$

$$\overline{q}^2$$

```
(*CartesianPair[CartesianMomentum[p], CartesianMomentum[q]]*)

(*CartesianPair[CartesianMomentum[p, -1 + D], CartesianMomentum[q, -1 + D]]*)
```

$$\frac{s}{2}$$

$$-\overline{k}\cdot \overline{p}_1+\overline{k}\cdot \overline{q}-\overline{q}\cdot \overline{p}_2+\frac{s}{2}$$

$$-\overline{k}\cdot \overline{p}_1+\overline{k}\cdot \overline{q}-\overline{q}\cdot \overline{p}_2+\frac{s}{2}$$