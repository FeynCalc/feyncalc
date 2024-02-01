## CLC

`CLC[m, n, r]` evaluates to `Eps[CartesianIndex[m], CartesianIndex[n], CartesianIndex[r]]` applying `FeynCalcInternal`.

`CLC[m,...][p, ...]` evaluates to `Eps[CartesianIndex[m], ..., CartesianMomentum[p], ...]` applying `FeynCalcInternal`.

When some indices of a Levi-Civita-tensor are contracted with 3-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3}$ (accessible via `CLC[][p1,p2,p3]`) correspond to $\varepsilon^{i j k} p_1^i p_2^j p_3^k$.

### See also

[Overview](Extra/FeynCalc.md), [LC](LC.md), [Eps](Eps.md).

### Examples

```mathematica
CLC[i, j, k]
```

$$\bar{\epsilon }^{ijk}$$

```mathematica
CLC[i, j, k] // FCI // StandardForm

(*Eps[CartesianIndex[i], CartesianIndex[j], CartesianIndex[k]]*)
```

```mathematica
CLC[i][p, q]
```

$$\bar{\epsilon }^{i\overline{p}\overline{q}}$$

```mathematica
CLC[i][p, q] // FCI // StandardForm

(*Eps[CartesianIndex[i], CartesianMomentum[p], CartesianMomentum[q]]*)
```

```mathematica
Contract[CLC[i, j, k] CLC[i, l, m]]
```

$$\bar{\delta }^{jl} \bar{\delta }^{km}-\bar{\delta }^{jm} \bar{\delta }^{kl}$$

```mathematica
CLC[i, j, k] CV[Subscript[p, 1], i] CV[Subscript[p, 2], j] CV[Subscript[p, 3], k] 
 
Contract[%]
```

$$\overline{p}_1{}^i \overline{p}_2{}^j \overline{p}_3{}^k \bar{\epsilon }^{ijk}$$

$$\bar{\epsilon }^{\overline{p}_1\overline{p}_2\overline{p}_3}$$