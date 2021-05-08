##  CLCD 

CLCD[m, n, r]  evaluates to Eps[CartesianIndex[m, D-1], CartesianIndex[n, D-1], CartesianIndex[r,D-1]] applying FeynCalcInternal. CLC[m,...][p, ...] evaluates to Eps[CartesianIndex[m, D-1], ..., CartesianMomentum[p, D-1], ...] applying FeynCalcInternal..

###  See also 

LCD, Eps.

###  Examples 

```mathematica
CLCD[i, j, k] 
 
% // FCI // StandardForm 
 
CLCD[i, j][p] 
 
% // FCI // StandardForm 
 
CLCD[i, j][p] CLCD[i, j][q] // Contract // Factor2
```

$$\overset{\text{}}{\epsilon }^{ijk}$$

```
(*Eps[CartesianIndex[i, -1 + D], CartesianIndex[j, -1 + D], CartesianIndex[k, -1 + D]]*)
```

$$\overset{\text{}}{\epsilon }^{ijp}$$

```
(*Eps[CartesianIndex[i, -1 + D], CartesianIndex[j, -1 + D], CartesianMomentum[p, -1 + D]]*)
```

$$(2-D) (3-D) (p\cdot q)$$