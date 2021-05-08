##  CGAD 

CGAD[] can be used as input for $\gamma ^i$ in D dimensions, where i is a Cartesian index, and is transformed into DiracGamma[CartesianIndex[i,D-1],D] by FeynCalcInternal.

###  See also 

GAD, DiracGamma.

###  Examples 

```mathematica
CGAD[i] 
 
CGAD[i, j] - CGAD[j, i] 
 
StandardForm[FCI[CGAD[i]]] 
 
CGAD[i, j, k, l] 
 
StandardForm[CGAD[i, j, k, l]] 
 
DiracSimplify[DiracTrace[CGAD[i, j, k, l]]] 
 
CGAD[i] . (CGSD[p] + m) . CGAD[j]
```

$$\gamma ^i$$

$$\gamma ^i.\gamma ^j-\gamma ^j.\gamma ^i$$

```
(*DiracGamma[CartesianIndex[i, -1 + D], D]*)
```

$$\gamma ^i.\gamma ^j.\gamma ^k.\gamma ^l$$

```
(*CGAD[i] . CGAD[j] . CGAD[k] . CGAD[l]*)
```

$$4 \delta ^{il} \delta ^{jk}-4 \delta ^{ik} \delta ^{jl}+4 \delta ^{ij} \delta ^{kl}$$

$$\gamma ^i.(m+\gamma \cdot p).\gamma ^j$$