## CLCD

`CLCD[m, n, r]`  evaluates to `Eps[CartesianIndex[m, D-1], CartesianIndex[n, D-1], CartesianIndex[r,D-1]]` applying `FeynCalcInternal`.

`CLC[m,...][p, ...]` evaluates to `Eps[CartesianIndex[m, D-1], ..., CartesianMomentum[p, D-1], ...]` applying `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [LCD](LCD.md), [Eps](Eps.md).

### Examples

```mathematica
CLCD[i, j, k]
% // FCI // StandardForm
```

$$\overset{\text{}}{\epsilon }^{ijk}$$

```
(*Eps[CartesianIndex[i, -1 + D], CartesianIndex[j, -1 + D], CartesianIndex[k, -1 + D]]*)
```

```mathematica
CLCD[i, j][p]
% // FCI // StandardForm
```

$$\overset{\text{}}{\epsilon }^{ijp}$$

```
(*Eps[CartesianIndex[i, -1 + D], CartesianIndex[j, -1 + D], CartesianMomentum[p, -1 + D]]*)
```

```mathematica
CLCD[i, j][p] CLCD[i, j][q] // Contract // Factor2
```

$$(2-D) (3-D) (p\cdot q)$$
