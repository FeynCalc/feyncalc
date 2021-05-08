##  TGA 

TGA[]  can be used as input for $\gamma ^0$ in 4 dimensions and is transformed into DiracGamma[ExplicitLorentzIndex[0]] by FeynCalcInternal.

###  Examples 

```mathematica
TGA[] 
 
TGA[] // FCI // StandardForm 
 
TGA[] . TGA[] // DiracSimplify
```

$$\bar{\gamma }^0$$

```
(*DiracGamma[ExplicitLorentzIndex[0]]*)
```

$$1$$