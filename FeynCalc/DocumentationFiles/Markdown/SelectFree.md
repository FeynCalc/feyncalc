`SelectFree[expr, a, b, ...]` is equivalent to `Select[expr, FreeQ2[#, {a,b, ...}]&]`, except the special cases: `SelectFree[a, b]` returns `a` and `SelectFree[a,a]` returns 1 (where `a` is not a product or a sum).

### See also

[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectNotFree](SelectNotFree.md).

### Examples

```mathematica
SelectFree[a + b + f[a] + d, a]
```

$$b+d$$

```mathematica
SelectFree[x y, x]
```

$$y$$

```mathematica
SelectFree[2 x y z f[x], {x, y}]
```

$$2 z$$

```mathematica
SelectFree[a, b]
```

$$a$$

```mathematica
SelectFree[a, a]
```

$$1$$

```mathematica
SelectFree[1, c]
```

$$1$$

```mathematica
SelectFree[f[x], x]
```

$$1$$
