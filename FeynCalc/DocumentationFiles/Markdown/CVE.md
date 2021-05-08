##  CVE 

CVE[p, i] is a D-4-dimensional Cartesian vector and is transformed into CartesianPair[CartesianMomentum[p,D-4], CartesianIndex[i,D-4]] by FeynCalcInternal..

###  See also 

FVE, Pair, CartesianPair.

###  Examples 

```mathematica
CVE[p, i] 
 
CVE[p - q, i] 
 
FCI[CVE[p, i]] // StandardForm
```

$$\hat{p}^i$$

$$\left(\hat{p}-\hat{q}\right)^i$$

```
(*CartesianPair[CartesianIndex[i, -4 + D], CartesianMomentum[p, -4 + D]]*)
```

ExpandScalarProduct is used to expand momenta in CVE

```mathematica
ExpandScalarProduct[CVE[p - q, i]]
```

$$\hat{p}^i-\hat{q}^i$$