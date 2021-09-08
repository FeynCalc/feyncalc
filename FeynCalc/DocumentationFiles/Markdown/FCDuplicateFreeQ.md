## FCDuplicateFreeQ

`FCDuplicateFreeQ[list]` yields `True` if list contains no duplicates and `False` otherwise.

`FCDuplicateFreeQ[list,test]` uses test to determine whether two objects should be considered duplicates.

`FCDuplicateFreeQ` returns the same results as the standard `DuplicateFreeQ`. The only reason for introducing `FCDuplicateFreeQ` is that `DuplicateFreeQ` is not available in Mathematica 8 and 9, which are still supported by FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [FCSubsetQ](FCSubsetQ.md).

### Examples

```mathematica
FCDuplicateFreeQ[{a, b, c}]
```

$$\text{True}$$

```mathematica
FCDuplicateFreeQ[{a, b, c, a}]
```

$$\text{False}$$

```mathematica
FCDuplicateFreeQ[{{a, b}, {a, c}}]
```

$$\text{True}$$

```mathematica
FCDuplicateFreeQ[{{a, b}, {a, c}}, Function[{x, y}, First[x] === First[y]]]
```

$$\text{False}$$
