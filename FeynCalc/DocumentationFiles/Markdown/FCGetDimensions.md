##  FCGetDimensions 

FCGetDimensions[expr] is an auxiliary function that determines the dimensions in which 4-momenta and Dirac matrices of the given expression are defined. The result is returned as a list, e.g. ${4}$, ${D}$ or ${4,D,D-4}$ etc. This is useful if one wants to be sure that all quantities inside a particular expression are purely $4$-dimensional or purely $text{D}$-dimensional..

###  Examples 

```mathematica
FCGetDimensions[GA[i]] 
 
FCGetDimensions[GSD[p]] 
 
FCGetDimensions[FVE[q, \[Mu]] GS[p]]
```

$$\{4\}$$

$$\{D\}$$

$$\{4,D-4\}$$