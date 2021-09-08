## FreeQ2

`FreeQ2[expr, {form1, form2, ...}]` yields `True` if `expr` does not contain any occurrence of `form1, form2, ...` and `False` otherwise.

`FreeQ2[expr, form]` is the same as `FreeQ[expr, form]`.

### See also

[Overview](Extra/FeynCalc.md), [SelectFree](SelectFree.md), [SelectNotFree](SelectNotFree.md).

### Examples

```mathematica
FreeQ2[x + f[x] + y, {a, x}]
```

$$\text{False}$$

```mathematica
FreeQ2[x + f[x] + y, {a, b}]
```

$$\text{True}$$

```mathematica
FreeQ2[x, y]
```

$$\text{True}$$

```mathematica
FreeQ2[f[x], f]
```

$$\text{False}$$
