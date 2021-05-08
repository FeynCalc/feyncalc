##  KDE 

KDE[i, j]  is the Kronecker delta in D-4 dimensions..

###  Examples 

```mathematica
KDE[i, j] 
 
Contract[KDE[i, j] KDE[i, j]] 
 
Contract[KDE[i, j] KD[i, j]] 
 
Contract[KDE[i, j] KDD[i, j]] 
 
KDE[i, j] // StandardForm 
 
FCI[KDE[i, j]] // StandardForm 
 
FCE[FCI[KDE[i, j]]] // StandardForm
```

$$\hat{\delta }^{ij}$$

$$D-4$$

$$0$$

$$D-4$$

```
(*KDE[i, j]*)

(*CartesianPair[CartesianIndex[i, -4 + D], CartesianIndex[j, -4 + D]]*)

(*KDE[i, j]*)
```