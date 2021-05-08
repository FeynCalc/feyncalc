##  CLC 

CLC[m, n, r] evaluates to Eps[CartesianIndex[m], CartesianIndex[n], CartesianIndex[r]] applying FeynCalcInternal. CLC[m,...][p, ...] evaluates to Eps[CartesianIndex[m], ..., CartesianMomentum[p], ...] applying FeynCalcInternal..

###  See also 

LC, Eps.

###  Examples 

```mathematica
CLC[i, j, k] 
 
% // FCI // StandardForm
```

$$\bar{\epsilon }^{ijk}$$

```
(*Eps[CartesianIndex[i], CartesianIndex[j], CartesianIndex[k]]*)
```

```mathematica
CLC[i][p, q] 
 
% // FCI // StandardForm 
 
Contract[CLC[i, j, k] CLC[i, l, m]]
```

$$\bar{\epsilon }^{i\overline{p}\overline{q}}$$

```
(*Eps[CartesianIndex[i], CartesianMomentum[p], CartesianMomentum[q]]*)
```

$$\bar{\delta }^{jl} \bar{\delta }^{km}-\bar{\delta }^{jm} \bar{\delta }^{kl}$$