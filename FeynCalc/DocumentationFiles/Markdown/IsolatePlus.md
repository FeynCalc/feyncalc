## IsolatePlus

`IsolatePlus` is an option for `Isolate` and other functions using `Isolate`. If it is set to `True`, Isolate will split sums that contain elements from `vlist`, to be able to abbreviate the `vlist`-free part.

### See also

[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md).

### Examples

```mathematica
Isolate[a + b + c + d, a] 
```

$$a+b+c+d$$

```mathematica
Isolate[a + b + c + d, a, IsolatePlus -> True]
```

$$a+\text{KK}(24)$$
