## Dimension

`Dimension` is an option of several functions and denotes the number of space-time dimensions. Possible settings are: `4`, `n`, `d`, `D`, ... , the variable does not matter, but it should have head Symbol.

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md).

### Examples

```mathematica
Options[ScalarProduct]
```

$$\{\text{Dimension}\to 4,\text{FeynCalcInternal}\to \;\text{True},\text{SetDimensions}\to \{4,D\}\}$$

```mathematica
ex = ScalarProduct[m, n, Dimension -> d]
```

$$m\cdot n$$

```mathematica
ex // StandardForm

(*Pair[Momentum[m, d], Momentum[n, d]]*)
```