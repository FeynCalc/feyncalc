## FCSetPauliSigmaScheme

`FCSetPauliSigmaScheme[scheme]` allows you to specify how Pauli matrices will be handled in $D-1$ dimensions.

This is mainly related to the commutator of two Pauli matrices, which involves a Levi-Civita tensor. The latter is not a well-defined quantity in $D-1$ dimensions. Following schemes are supported:

- `"None"` - This is the default value. The anticommutator relation is not applied to $D-1$ dimensional Pauli matrices.

- `"Naive"` - Naively apply the commutator relation in $D-1$-dimensions, i.e.  $\{\sigma^i, \sigma^j \} = 2 i \varepsilon^{ijk} \sigma^k$. The Levi-Civita tensor lives in $D-1$-dimensions, so that a contraction of two such tensors which have all indices in common yields $(D-3) (D-2) (D-1)$.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [FCGetPauliSigmaScheme](FCGetPauliSigmaScheme.md).

### Examples

```mathematica
FCGetPauliSigmaScheme[]
```

$$\text{None}$$

```mathematica
CSID[i, j, k]
PauliSimplify[%, PauliReduce -> True]
```

$$\sigma ^i.\sigma ^j.\sigma ^k$$

$$\sigma ^i.\sigma ^j.\sigma ^k$$

```mathematica
FCSetPauliSigmaScheme["Naive"];
```

```mathematica
FCGetPauliSigmaScheme[]
```

$$\text{Naive}$$

```mathematica
PauliSimplify[CSID[i, j, k], PauliReduce -> True]
% // FCE // StandardForm
```

$$i \overset{\text{}}{\epsilon }^{ijk}+D \sigma ^i \delta ^{jk}-D \sigma ^j \delta ^{ik}-3 \sigma ^i \delta ^{jk}+3 \sigma ^j \delta ^{ik}+\sigma ^k \delta ^{ij}$$

```
(*I CLCD[i, j, k] + CSID[k] KDD[i, j] + 3 CSID[j] KDD[i, k] - D CSID[j] KDD[i, k] - 3 CSID[i] KDD[j, k] + D CSID[i] KDD[j, k]*)
```

```mathematica
FCSetPauliSigmaScheme["None"];
```
