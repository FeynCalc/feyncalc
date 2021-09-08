## SelectFree2

`SelectFree2[expr, a, b, ...]` is similar to `SelectFree` but it also differs from the latter in several respects.

If `expr` is  a list, `SelectFree2` behaves exactly the same way as `SelectFree`.

If `expr` is not a list, `SelectFree2` first expands the expression w.r.t. the arguments via `Expand2`.

Furthermore, `SelectFree2[a,b]` returns `a` and `SelectFree2[a,a]` returns `0`. This differs from the behavior of `SelectFree` but is consistent with the naive expectations when applying the function to a sum of terms.

### See also

[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectFree](SelectFree.md), [SelectNotFree](SelectNotFree.md), [SelectNotFree2](SelectNotFree2.md).

### Examples

Note the difference between SelectFree and SelectFree2

```mathematica
SelectFree[(a + b) c, b]
```

$$c$$

```mathematica
SelectFree2[(a + b) c, b]
```

$$a c$$

```mathematica
SelectFree[a, b]
```

$$a$$

```mathematica
SelectFree2[a, b]
```

$$a$$

```mathematica
SelectFree[a, a]
```

$$1$$

```mathematica
SelectFree2[a, a]
```

$$0$$

When there are hidden zeros, `SelectFree2` obviously works better

```mathematica
SelectFree[(a - b + c)^2 - (a^2 - 2 a b + 2 a c + b^2 - 2 b c + c^2),a]
```

$$-b^2+2 b c-c^2$$

```mathematica
SelectFree2[(a - b + c)^2 - (a^2 - 2 a b + 2 a c + b^2 - 2 b c + c^2),a]
```

$$0$$
