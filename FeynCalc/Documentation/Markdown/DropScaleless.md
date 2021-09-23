## DropScaleless

`DropScaleless` is an option for `FCLoopIsolate`, `ApartFF`, `FourDivergence` and other functions. When set to `True`, all loop integrals that do not contain a `FeynAmpDenominator`, i.e. consist of only scalar products but no denominators, are set to zero.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopIsolate](FCLoopIsolate.md).

### Examples

```mathematica
FCLoopIsolate[SPD[l, q], {q}]
```

$$\text{FCGV}(\text{LoopInt})(l\cdot q)$$

```mathematica
FCLoopIsolate[SPD[l, q], {q}, DropScaleless -> True]
```

$$0$$
