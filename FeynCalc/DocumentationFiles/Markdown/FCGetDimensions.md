## FCGetDimensions

`FCGetDimensions[expr]` is an auxiliary function that determines the dimensions in which 4-momenta and Dirac matrices of the given expression are defined. The result is returned as a list, e.g. `{4}`, `{D}` or `{4,D,D-4}` etc.

This is useful if one wants to be sure that all quantities inside a particular expression are purely $4$-dimensional or purely $D$-dimensional.

### See also

[Overview](Extra/FeynCalc.md), [ChangeDimension](ChangeDimension.md).

### Examples

```mathematica
FCGetDimensions[GA[i]]
```

$$\{4\}$$

```mathematica
FCGetDimensions[GSD[p]]
```

$$\{D\}$$

```mathematica
FCGetDimensions[FVE[q, \[Mu]] GS[p]]
```

$$\{4,D-4\}$$
