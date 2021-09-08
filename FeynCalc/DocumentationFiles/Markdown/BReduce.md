## BReduce

`BReduce` is an option for `B0`, `B00`, `B1`, `B11` determining whether reductions to `A0` and `B0` will be done.

### See also

[Overview](Extra/FeynCalc.md), [A0](A0.md), [B0](B0.md), [B00](B00.md), [B1](B1.md), [B11](B11.md).

### Examples

By default $B_0$ is not expressed in terms of $A_0$.

```mathematica
B0[0, s, s]
```

$$\text{B}_0(0,s,s)$$

With BReduceTrue, transformation is done.

```mathematica
B0[0, s, s, BReduce -> True]
```

$$-\frac{(2-D) \;\text{A}_0(s)}{2 s}$$
