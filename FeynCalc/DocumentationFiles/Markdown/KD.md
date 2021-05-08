##  KD 

KD[i, j]  is the Kronecker delta in 3 dimensions..

###  Examples 

```mathematica
KD[i, j] 
 
Contract[KD[i, j] KD[i, j]] 
 
KD[a, b] // StandardForm 
 
FCI[KD[a, b]] // StandardForm 
 
FCE[FCI[KD[a, b]]] // StandardForm
```

$$\bar{\delta }^{ij}$$

$$3$$

```
(*KD[a, b]*)

(*CartesianPair[CartesianIndex[a], CartesianIndex[b]]*)

(*KD[a, b]*)
```