## FCReplaceAll

`FCReplaceAll[exp, ru1, ...]` is like `ReplaceAll`, but it also allows to apply multiple replacement rules sequentially. Instead of doing `exp /. ru1 /. ru2 /. ru3` one can just write `FCReplaceAll[exp, ru1, ru2, ru3]`.

### See also

[Overview](Extra/FeynCalc.md), [FCReplaceRepeated](FCReplaceRepeated.md).

### Examples

```mathematica
FCReplaceAll[a, a -> b]
```

$$b$$

```mathematica
FCReplaceAll[a c, {a -> b, c -> d}]
```

$$b d$$

```mathematica
FCReplaceAll[a c, a -> b, c -> d, d -> e, b -> f]
```

$$e f$$
