## SelectNotFree2

`SelectNotFree2[expr, a, b, ...]` is like `SelectNotFree` but it first expands the expression w.r.t. the arguments via `Expand2`.

### See also

[FreeQ2](FreeQ2), [SelectFree](SelectFree), [SelectNotFree](SelectNotFree), [SelectFree2](SelectFree2).

### Examples

```mathematica
SelectNotFree[(a + b) c, b]
```

$$a+b$$

```mathematica
SelectNotFree2[(a + b) c, b]
```

$$b c$$