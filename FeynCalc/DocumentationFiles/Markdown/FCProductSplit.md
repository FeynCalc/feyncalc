## FCProductSplit

`FCProductSplit[exp, {v1, v2, ...}]` splits expr into pieces that are free of any occurrence of `v1, v2, ...` and pieces that contain those variables. This works both on sums and products. The output is provided in the form of a two element list. One can recover the original expression by applying `Total` to that list.

### See also

[Overview](Extra/FeynCalc.md), [FCSplit](FCSplit.md).

### Examples

```mathematica
FCProductSplit[c^2, {a}]
```

$$\left\{c^2,1\right\}$$

```mathematica
FCProductSplit[a^2*b, {a}]
```

$$\left\{b,a^2\right\}$$

```mathematica
FCProductSplit[(a^2 + b)*b*(c + d), {a, c}]
```

$$\left\{b,\left(a^2+b\right) (c+d)\right\}$$
