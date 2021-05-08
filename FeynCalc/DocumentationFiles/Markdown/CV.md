##  CV 

CV[p, i] is a 3-dimensional Cartesian vector and is transformed into CartesianPair[CartesianMomentum[p], CartesianIndex[i]] by FeynCalcInternal..

###  See also 

FV, Pair, CartesianPair.

###  Examples 

```mathematica
CV[p, i] 
 
CV[p - q, i] 
 
FCI[CV[p, i]] // StandardForm
```

$$\overline{p}^i$$

$$\left(\overline{p}-\overline{q}\right)^i$$

```
(*CartesianPair[CartesianIndex[i], CartesianMomentum[p]]*)
```

ExpandScalarProduct is used to expand momenta in CV

```mathematica
ExpandScalarProduct[CV[p - q, i]]
```

$$\overline{p}^i-\overline{q}^i$$