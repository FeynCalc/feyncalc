## SUNNToCACF

`SUNNToCACF` is an option of `SUNSimplify` and `CalcColorFactor`. If set to `True`, the Casimir operator eigenvalues `CA` ($=n_c$) and `CF` ($=(n_c^2-1)/(2 n_c)$) are introduced.

### See also

[Overview](Extra/FeynCalc.md), [CalcColorFactor](CalcColorFactor.md), [SUNSimplify](SUNSimplify.md), [Trick](Trick.md), [SUNN](SUNN.md), [CA](CA.md), [CF](CF.md).

### Examples

```mathematica
SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]], SUNNToCACF -> True]
```

$$2 C_A C_F$$
