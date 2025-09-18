## CommutatorOrder

`CommutatorOrder[exp]` orders any `FCCommutator` and `FCAntiCommutator` lexicographically.

### See also

[Overview](Extra/FeynCalc.md), [FCCommutator](FCCommutator.md), [FCAntiCommutator](FCAntiCommutator.md).

### Examples

```mathematica
FCCommutator[a, b] + FCCommutator[b, a] 
 
CommutatorOrder[%]
```

$$[a,b]+[b,a]$$

$$0$$