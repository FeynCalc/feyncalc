## Variables2

`Variables2[expr]` is like `Variables`, but it also works on rules and equalities as well as lists thereof.

`Variables2` always applies `Union` to the output.

### See also

[Overview](Extra/FeynCalc.md), [Cases2](Cases2.md).

### Examples

Some cases where `Variables2` is much more useful than `Variables`

```mathematica
Variables[{a -> x1 + y1, b -> x2 + y2}]
```

$$\{\}$$

```mathematica
Variables2[{a -> x1 + y1, b -> x2 + y2}]
```

$$\{a,b,\text{x1},\text{x2},\text{y1},\text{y2}\}$$

```mathematica
Variables[a + b == c + d]
```

$$\{\}$$

```mathematica
Variables2[a + b == c + d]
```

$$\{a,b,c,d\}$$
