## UnDeclareFCTensor

`UnDeclareFCTensor[a, b, ...]` undeclares `a,b, ...` to be tensor heads, i.e., `DataType[a,b, ...,  FCTensor]` is set to `False`.

### See also

[Overview](Extra/FeynCalc.md), [DeclareFCTensor](DeclareFCTensor.md), [FCTensor](FCTensor.md).

### Examples

```mathematica
ClearAll[myTens]
DeclareFCTensor[myTens]
ExpandScalarProduct[myTens[z, Momentum[a + b], Momentum[c + d]]]
```

$$\text{myTens}\left(z,\overline{a},\overline{c}\right)+\text{myTens}\left(z,\overline{a},\overline{d}\right)+\text{myTens}\left(z,\overline{b},\overline{c}\right)+\text{myTens}\left(z,\overline{b},\overline{d}\right)$$

```mathematica
UnDeclareFCTensor[myTens]
```
