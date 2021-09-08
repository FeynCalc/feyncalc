## PowerSimplify

`PowerSimplify[exp]` simplifies `(-x)^a` to `(-1)^a x^a` and `(y-x)^n` to `(-1)^n (x-y)^n` thus assuming that the exponent is an integer (even if it is symbolic).

Furthermore, `(-1)^(a+n) ` and `I^(a+n)` are expanded and `(I)^(2 m) -> (-1)^m and (-1)^(n_Integer?EvenQ m) -> 1` and `(-1)^(n_Integer?OddQ m) -> (-1)^m` for `n` even and odd respectively and (-1)^(-n) -> (-1)^n and Exp[I m Pi] -> (-1)^m.

### See also

[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [OPEm](OPEm.md).

### Examples

```mathematica
PowerSimplify[(-1)^(2 OPEm)]
```

$$1$$

```mathematica
PowerSimplify[(-1)^(OPEm + 2)]
```

$$(-1)^m$$

```mathematica
PowerSimplify[(-1)^(OPEm - 2)]
```

$$(-1)^m$$

```mathematica
PowerSimplify[I^(2 OPEm)]
```

$$(-1)^m$$