## SelectNotFree

`SelectNotFree[expr, x]` returns that part of `expr` which is not free of any occurrence of `x`.

`SelectNotFree[expr, a, b, ...]` is equivalent to `Select[expr, !FreeQ2[#, {a, b, ...}]&]`, except the special cases:
`SelectNotFree[a, b]` returns `1` and `SelectNotFree[a, a]` returns `a` (where `a` is not a product or a sum).

### See also

[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectFree](SelectFree.md).

### Examples

```mathematica
SelectNotFree[a + b + f[a], a]
```

$$f(a)+a$$

```mathematica
SelectNotFree[2 x y f[x] z, {x, y}]
```

$$x y f(x)$$

```mathematica
SelectNotFree[a, b]
```

$$1$$

```mathematica
SelectNotFree[a + x, b]
```

$$0$$

```mathematica
SelectNotFree[a, a]
```

$$a$$

```mathematica
SelectNotFree[1, c]
```

$$1$$

```mathematica
SelectNotFree[f[x], x]
```

$$f(x)$$