`ExpandAll2[exp]` is similar to `ExpandAll`, but much faster on simple structures.

### See also

### Examples

Benchmark against the standard ExpandAll

```mathematica
exp = Sum[p[i], {i, 1, 100}] Sum[q[i], {i, 1, 1000}];
```

```mathematica
AbsoluteTiming[res1 = ExpandAll[exp];]
```

$$\{0.437121,\text{Null}\}$$

```mathematica
AbsoluteTiming[res2 = ExpandAll2[exp];]
```

$$\{0.183987,\text{Null}\}$$

```mathematica
res1 === res2
```

$$\text{True}$$

```mathematica
ClearAll[exp, res1, res2]
```