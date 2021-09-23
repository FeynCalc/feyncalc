## FCSubsetQ

`FCSubsetQ[list1, list2]`  yields `True` if `list2` is a subset of `list1` and `False` otherwise. It returns the same results as the standard `SubsetQ`. The only reason for introducing `FCSubsetQ` is that `SubsetQ` is not available in Mathematica 8 and 9, which are still supported by FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [FCDuplicateFreeQ](FCDuplicateFreeQ.md).

### Examples

```mathematica
FCSubsetQ[{a, b, c, d}, {a, d, e}]
```

$$\text{False}$$

```mathematica
FCSubsetQ[{a, b, c, d}, {a, d}]
```

$$\text{True}$$
