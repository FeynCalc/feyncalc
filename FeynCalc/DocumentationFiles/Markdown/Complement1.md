## Complement1

`Complement1[l1, l2]` where `l1` and `l2` are lists returns a list of elements from `l1` not in`l2`. Multiple occurrences of an element in `l1` are kept and multiple occurrences of an element in `l2` are dropped if present in `l1`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
Complement[{a, b, c, d, e, f, e}, {a, b, c, d}]
```

$$\{e,f\}$$

```mathematica
Complement1[{a, b, c, d, e, f, e}, {a, b, c, d}]
```

$$\{e,f,e\}$$
