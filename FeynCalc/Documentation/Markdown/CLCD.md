## CLCD

`CLCD[m, n, r]`  evaluates to `Eps[CartesianIndex[m, D-1], CartesianIndex[n, D-1], CartesianIndex[r,D-1]]` applying `FeynCalcInternal`.

`CLC[m,...][p, ...]` evaluates to `Eps[CartesianIndex[m, D-1], ..., CartesianMomentum[p, D-1], ...]` applying `FeynCalcInternal`.

When some indices of a Levi-Civita-tensor are contracted with 3-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3}$ (accessible via `CLCD[][p1,p2,p3]`) correspond to $\varepsilon^{i j k} p_1^i p_2^j p_3^k$.

### See also

[Overview](Extra/FeynCalc.md), [LCD](LCD.md), [Eps](Eps.md).

### Examples

```mathematica
CLCD[i, j, k]
```

$$\overset{\text{}}{\epsilon }^{ijk}$$

```mathematica
CLCD[i, j, k] // FCI // StandardForm

(*Eps[CartesianIndex[i, -1 + D], CartesianIndex[j, -1 + D], CartesianIndex[k, -1 + D]]*)
```

```mathematica
CLCD[i, j][p]
```

$$\overset{\text{}}{\epsilon }^{ijp}$$

```mathematica
CLCD[i, j][p] // FCI // StandardForm

(*Eps[CartesianIndex[i, -1 + D], CartesianIndex[j, -1 + D], CartesianMomentum[p, -1 + D]]*)
```

```mathematica
CLCD[i, j][p] CLCD[i, j][q] // Contract // Factor2
```

$$(2-D) (3-D) (p\cdot q)$$

```mathematica
CLCD[i, j, k] CVD[Subscript[p, 1], i] CVD[Subscript[p, 2], j] CVD[Subscript[p, 3], k] 
 
Contract[%]
```

$$p_1{}^i p_2{}^j p_3{}^k \overset{\text{}}{\epsilon }^{ijk}$$

$$\overset{\text{}}{\epsilon }^{p_1p_2p_3}$$