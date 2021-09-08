## FCReplaceRepeated

`FCReplaceRepeated[exp, ru1, ...]`  is like `ReplaceRepeated`, but it also allows to apply multiple replacement rules sequentially.

Instead of doing `exp //. ru1 //. ru2 //. ru3` one can just write `FCReplaceRepeated[exp, ru1, ru2, ru3]`.

### See also

[Overview](Extra/FeynCalc.md), [FCReplaceAll](FCReplaceAll.md).

### Examples

```mathematica
FCReplaceRepeated[a, a -> b]
```

$$b$$

```mathematica
FCReplaceRepeated[a c, {a -> b, c -> d}]
```

$$b d$$

```mathematica
FCReplaceRepeated[a c, a -> b, c -> d]
```

$$b d$$

```mathematica
FCReplaceRepeated[a c, a -> b, c -> d, d -> e, b -> f]
```

$$e f$$
