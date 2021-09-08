## ExpandAll2

`ExpandAll2[exp]` is similar to `ExpandAll`, but much faster on simple structures.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

Benchmark against the standard ExpandAll

```mathematica
exp = Sum[p[i], {i, 1, 100}] Sum[q[i], {i, 1, 1000}];
```

```mathematica
AbsoluteTiming[res1 = ExpandAll[exp];]
```

$$\{0.371317,\text{Null}\}$$

```mathematica
AbsoluteTiming[res2 = ExpandAll2[exp];]
```

$$\{0.150584,\text{Null}\}$$

```mathematica
res1 === res2
```

$$\text{True}$$

```mathematica
ClearAll[exp, res1, res2]
```
