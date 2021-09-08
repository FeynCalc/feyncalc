## MLimit

`MLimit[expr, lims]` takes multiple limits of `expr` using the limits `lims`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
MLimit[y Log[y] + Sin[x - 1]/(x - 1), {x -> 1, y -> 0}]
```

$$1$$
