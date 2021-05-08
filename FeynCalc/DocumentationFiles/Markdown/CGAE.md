##  CGAE 

CGAE[i] can be used as input for $\gamma ^i$ in D-4 dimensions, where i is a Cartesian index, and is transformed into DiracGamma[CartesianIndex[i,D-4],D-4] by FeynCalcInternal.

###  See also 

GAE, DiracGamma.

###  Examples 

```mathematica
CGAE[i] 
 
CGAE[i, j] - CGAE[j, i] 
 
StandardForm[FCI[CGAE[i]]] 
 
CGAE[i, j, k, l] 
 
StandardForm[CGAE[i, j, k, l]]
```

$$\hat{\gamma }^i$$

$$\hat{\gamma }^i.\hat{\gamma }^j-\hat{\gamma }^j.\hat{\gamma }^i$$

```
(*DiracGamma[CartesianIndex[i, -4 + D], -4 + D]*)
```

$$\hat{\gamma }^i.\hat{\gamma }^j.\hat{\gamma }^k.\hat{\gamma }^l$$

```
(*CGAE[i] . CGAE[j] . CGAE[k] . CGAE[l]*)
```