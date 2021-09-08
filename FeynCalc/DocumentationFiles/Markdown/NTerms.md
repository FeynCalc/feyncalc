## NTerms

`NTerms[x]` is equivalent to `Length` if `x` is a sum; otherwise `NTerms[x]` returns `1`, except `NTerms[0] -> 0`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
NTerms[a - b]
```

$$2$$

```mathematica
NTerms[a b c]
```

$$1$$

```mathematica
NTerms[9]
```

$$1$$

```mathematica
NTerms[0]
```

$$0$$
