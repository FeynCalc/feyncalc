## Dimension

`Dimension` is an option of several functions and denotes the number of space-time dimensions. Possible settings are: `4`, `n`, `d`, `D`, ... , the variable does not matter, but it should have head Symbol.

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md).

### Examples

```mathematica
Options[ScalarProduct]
ScalarProduct[m, n, Dimension -> d]
% // StandardForm
```

$$\{\text{Dimension}\to 4,\text{FeynCalcInternal}\to \;\text{True},\text{SetDimensions}\to \{4,D\}\}$$

$$m\cdot n$$

```
(*Pair[Momentum[m, d], Momentum[n, d]]*)
```
