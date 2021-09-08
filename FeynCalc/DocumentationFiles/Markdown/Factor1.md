## Factor1

`Factor1[poly]` factorizes common terms  in the summands of poly. It uses basically `PolynomialGCD`.

### See also

[Overview](Extra/FeynCalc.md), [Factor2](Factor2.md).

### Examples

```mathematica
(a - x) (b - x)
{Factor1[%], Factor[%]}
```

$$(a-x) (b-x)$$

$$\{(a-x) (b-x),-((a-x) (x-b))\}$$

```mathematica
ex = Expand[(a - b) (a + b)]
```

$$a^2-b^2$$

```mathematica
Factor[ex]
```

$$(a-b) (a+b)$$

```mathematica
Factor1[ex]
```

$$a^2-b^2$$
