`SelectFree2[expr, a, b, ...]` is like `SelectFree` but it first expands the expression w.r.t. the arguments via `Expand2`.

### See also

[FreeQ2](FreeQ2), [SelectFree](SelectFree), [SelectNotFree](SelectNotFree), [SelectNotFree2](SelectNotFree2).

### Examples

```mathematica
SelectFree[(a + b) c, b]
```

$$c$$

```mathematica
SelectFree2[(a + b) c, b]
```

$$a c$$