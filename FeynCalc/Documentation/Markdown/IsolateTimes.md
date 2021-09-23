## IsolateTimes

`IsolateTimes` is an option for `Isolate` and other functions using `Isolate`. If it is set to `True`, Isolate will be applied also to pure products.

### See also

[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md), [Collect2](Collect2.md).

### Examples

By default, this expression does not become abbreviated

```mathematica
Isolate[a*b*c*d, a]
```

$$a b c d$$

Now an abbreviation is introduced

```mathematica
 Isolate[a*b*c*d, a, IsolateTimes -> True]
```

$$a \;\text{KK}(24)$$
