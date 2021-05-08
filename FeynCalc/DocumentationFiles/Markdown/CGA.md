##  CGA 

CGA[i] can be used as input for $\gamma ^i$ in 4 dimensions, where i is a Cartesian index, and is transformed into DiracGamma[CartesianIndex[i]] by FeynCalcInternal.

###  See also 

GA, DiracGamma.

###  Examples 

```mathematica
CGA[i] 
 
CGA[i, j] - CGA[j, i] 
 
StandardForm[FCI[CGA[i]]] 
 
CGA[i, j, k, l] 
 
StandardForm[CGA[i, j, k, l]] 
 
DiracSimplify[DiracTrace[CGA[i, j, k, l]]] 
 
CGA[i] . (CGS[p] + m) . CGA[j]
```

$$\overline{\gamma }^i$$

$$\overline{\gamma }^i.\overline{\gamma }^j-\overline{\gamma }^j.\overline{\gamma }^i$$

```
(*DiracGamma[CartesianIndex[i]]*)
```

$$\overline{\gamma }^i.\overline{\gamma }^j.\overline{\gamma }^k.\overline{\gamma }^l$$

```
(*CGA[i] . CGA[j] . CGA[k] . CGA[l]*)
```

$$4 \bar{\delta }^{il} \bar{\delta }^{jk}-4 \bar{\delta }^{ik} \bar{\delta }^{jl}+4 \bar{\delta }^{ij} \bar{\delta }^{kl}$$

$$\overline{\gamma }^i.\left(\overline{\gamma }\cdot \overline{p}+m\right).\overline{\gamma }^j$$