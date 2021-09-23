## SelectNotFree2

`SelectNotFree2[expr, a, b, ...]` is similar to `SelectNotFree` but it also differs from the latter in several respects.

If `expr` is  a list, `SelectNotFree2` behaves exactly the same way as `SelectNotFree`.

If `expr` is not a list, `SelectNotFree2` first expands the expression w.r.t. the arguments via `Expand2`.

Furthermore, `SelectNotFree2[a,b]` returns `0`. This differs from the behavior of `SelectFree` but is consistent with the naive expectations when applying the function to a sum of terms.

### See also

[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectFree](SelectFree.md), [SelectNotFree](SelectNotFree.md), [SelectFree2](SelectFree2.md).

### Examples

Note the difference between SelectNotFree and SelectNotFree2

```mathematica
SelectNotFree[(a + b) c, b]
```

$$a+b$$

```mathematica
SelectNotFree2[(a + b) c, b]
```

$$b c$$

```mathematica
SelectNotFree[a, b]
```

$$1$$

```mathematica
SelectNotFree2[a, b]
```

$$0$$

Here the behavior is identical

```mathematica
SelectNotFree[a, a]
```

$$a$$

```mathematica
SelectNotFree2[a, a]
```

$$a$$

When there are hidden zeros, `SelectNotFree2` obviously works better

```mathematica
SelectNotFree[(a - b + c)^2 - (a^2 - 2 a b + 2 a c + b^2 - 2 b c + c^2), a]
```

$$-a^2+(a-b+c)^2+2 a b-2 a c$$

```mathematica
SelectNotFree2[(a - b + c)^2 - (a^2 - 2 a b + 2 a c + b^2 - 2 b c + c^2), a]
```

$$0$$
