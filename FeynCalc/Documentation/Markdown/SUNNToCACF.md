## SUNNToCACF

`SUNNToCACF` is an option of `SUNSimplify` and `CalcColorFactor`. If set to `True`, the Casimir operator eigenvalues `CA` ($=n_c$) and `CF` ($=(n_c^2-1)/(2 n_c)$) are reconstructed from the result in terms of $n_c$ using heuristics. The reconstruction is not always perfect, but mostly sufficient at tree and one-loop level.

### See also

[Overview](Extra/FeynCalc.md), [SUNSimplify](SUNSimplify.md), [SUNN](SUNN.md), [CA](CA.md), [CF](CF.md).

### Examples

```mathematica
SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]]]
```

$$2 C_A C_F$$

```mathematica
SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]], SUNNToCACF -> False]
```

$$N^2-1$$