##  CSID 

CSID[i] can be used as input for D-1-dimensional $\sigma ^i$ with D-1-dimensional Cartesian index i and is transformed into PauliSigma[CartesianIndex[i,D-1],D-1] by FeynCalcInternal..

###  See also 

PauliSigma.

###  Examples 

```mathematica
CSID[i] 
 
CSID[i, j] - CSID[j, i] 
 
StandardForm[FCI[CSID[i]]] 
 
CSID[i, j, k, l] 
 
StandardForm[CSID[i, j, k, l]]
```

$$\sigma ^i$$

$$\sigma ^i.\sigma ^j-\sigma ^j.\sigma ^i$$

```
(*PauliSigma[CartesianIndex[i, -1 + D], -1 + D]*)
```

$$\sigma ^i.\sigma ^j.\sigma ^k.\sigma ^l$$

```
(*CSID[i] . CSID[j] . CSID[k] . CSID[l]*)
```