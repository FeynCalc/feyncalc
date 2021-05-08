##  CartesianPropagatorDenominator 

CartesianPropagatorDenominator[CartesianMomentum[q1, D - 1] +..., CartesianPair[CartesianMomentum[q1, D - 1], CartesianMomentum[p1, D - 1] +..., m^2, {n, s}] encodes a generic Cartesian propagator denominator 1/[(q1+...)^2 + q1.p1 + ... + m^2 + s*I eta]^n. CartesianPropagatorDenominator is an internal object. To enter such propagators in FeynCalc you should use CFAD..

###  See also 

CFAD, FeynAmpDenominator.

###  Examples 

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, m^2, {1, -1}]] 
 
FeynAmpDenominator[CartesianPropagatorDenominator[0, CartesianPair[CartesianMomentum[p, D - 1], CartesianMomentum[q, D - 1]], m^2, {1, -1}]]
```

$$![0q8mzf7tksbzy](img/0q8mzf7tksbzy.png)$$

$$\frac{1}{(p\cdot q+m^2-i \eta )}$$