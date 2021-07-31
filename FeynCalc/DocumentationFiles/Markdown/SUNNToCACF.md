## SUNNToCACF

`SUNNToCACF` is an option of `SUNSimplify` and `CalcColorFactor`. If set to `True`, the Casimir operator eigenvalues `CA` ($=n_c$) and `CF` ($=(n_c^2-1)/(2 n_c)$) are introduced.

### See also

[CalcColorFactor](CalcColorFactor), [SUNSimplify](SUNSimplify), [Trick](Trick), [SUNN](SUNN), [CA](CA), [CF](CF).

### Examples

```mathematica
SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]], SUNNToCACF -> True]
```

$$2 C_A C_F$$