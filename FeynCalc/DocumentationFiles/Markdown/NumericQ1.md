`NumericQ1[x, {a, b, ..}]` is like `NumericQ`, but assumes that `{a,b,..}` are numeric quantities.

### See also

### Examples

```mathematica
NumericQ[3 a + Log[b] + c^2]
```

$$\text{False}$$

```mathematica
NumericQ1[3 a + Log[b] + c^2, {}]
```

$$\text{False}$$

```mathematica
NumericQ1[3 a + Log[b] + c^2, {a, b, c}]
```

$$\text{True}$$