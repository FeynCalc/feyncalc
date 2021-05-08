##  CGSE 

CGSE[p] is transformed into DiracGamma[CartesianMomentum[p, D-4], D-4] by FeynCalcInternal. CGSE[p,q, ...] is equivalent to CGSE[p].CGSE[q]. ....

###  See also 

GSE, DiracGamma.

###  Examples 

```mathematica
CGSE[p] 
 
CGSE[p] // FCI // StandardForm 
 
CGSE[p, q, r, s] 
 
CGSE[p, q, r, s] // StandardForm 
 
CGSE[q] . (CGSE[p] + m) . CGSE[q]
```

$$\hat{\gamma }\cdot \hat{p}$$

```
(*DiracGamma[CartesianMomentum[p, -4 + D], -4 + D]*)
```

$$\left(\hat{\gamma }\cdot \hat{p}\right).\left(\hat{\gamma }\cdot \hat{q}\right).\left(\hat{\gamma }\cdot \hat{r}\right).\left(\hat{\gamma }\cdot \hat{s}\right)$$

```
(*CGSE[p] . CGSE[q] . CGSE[r] . CGSE[s]*)
```

$$\left(\hat{\gamma }\cdot \hat{q}\right).\left(m+\hat{\gamma }\cdot \hat{p}\right).\left(\hat{\gamma }\cdot \hat{q}\right)$$