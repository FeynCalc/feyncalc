## CLC

`CLC[m, n, r]` evaluates to `Eps[CartesianIndex[m], CartesianIndex[n], CartesianIndex[r]]` applying `FeynCalcInternal`.

`CLC[m,...][p, ...]` evaluates to `Eps[CartesianIndex[m], ..., CartesianMomentum[p], ...]` applying `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [LC](LC.md), [Eps](Eps.md).

### Examples

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
```

$$\bar{\epsilon }^{i\overline{p}\overline{q}}$$

```
(*Eps[CartesianIndex[i], CartesianMomentum[p], CartesianMomentum[q]]*)
```

```mathematica
Contract[CLC[i, j, k] CLC[i, l, m]]
```

$$\bar{\delta }^{jl} \bar{\delta }^{km}-\bar{\delta }^{jm} \bar{\delta }^{kl}$$
