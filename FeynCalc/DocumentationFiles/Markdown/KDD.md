##  KDD 

KDD[i, j] is the Kronecker delta in D-1 dimensions.

###  Examples 

```mathematica
KDD[i, j] 
 
Contract[KDD[i, j] KDD[i, j]] 
 
KDD[a, b] // StandardForm 
 
FCI[KDD[a, b]] // StandardForm 
 
FCE[FCI[KDD[a, b]]] // StandardForm
```

$$\delta ^{ij}$$

$$D-1$$

```
(*KDD[a, b]*)

(*CartesianPair[CartesianIndex[a, -1 + D], CartesianIndex[b, -1 + D]]*)

(*KDD[a, b]*)
```