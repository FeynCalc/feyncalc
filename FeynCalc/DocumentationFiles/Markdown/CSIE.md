##  CSIE 

CSIE[i] can be used as input for D-4-dimensional $\sigma ^i$ with D-4-dimensional Cartesian index i and is transformed into PauliSigma[CartesianIndex[i,D-4],D-4] by FeynCalcInternal..

###  See also 

PauliSigma.

###  Examples 

```mathematica
CSIE[i] 
 
CSIE[i, j] - CSIE[j, i] 
 
StandardForm[FCI[CSIE[i]]] 
 
CSIE[i, j, k, l] 
 
StandardForm[CSIE[i, j, k, l]]
```

$$\hat{\sigma }^i$$

$$\hat{\sigma }^i.\hat{\sigma }^j-\hat{\sigma }^j.\hat{\sigma }^i$$

```
(*PauliSigma[CartesianIndex[i, -4 + D], -4 + D]*)
```

$$\hat{\sigma }^i.\hat{\sigma }^j.\hat{\sigma }^k.\hat{\sigma }^l$$

```
(*CSIE[i] . CSIE[j] . CSIE[k] . CSIE[l]*)
```