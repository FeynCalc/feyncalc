## SUNTrace

`SUNTrace[exp]` is the head of color traces. By default the trace is not evaluated. The evaluation occurs only when the option `SUNTraceEvaluate` is set to `True`. It is recommended to use `SUNSimplify`, which will automatically evaluate all color traces involving 2 or 3 matrices in the input expression.

### See also

[Overview](Extra/FeynCalc.md), [SUNSimplify](SUNSimplify.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNF](SUNF.md), [SUND](SUND.md), [SUNTraceEvaluate](SUNTraceEvaluate.md).

### Examples

```mathematica
SUNTrace[SUNT[a, b]]
```

$$\text{tr}\left(T^a.T^b\right)$$

```mathematica
SUNTrace[SUNT[a, b], SUNTraceEvaluate -> True]
```

$$\frac{\delta ^{ab}}{2}$$

```mathematica
SUNTrace[SUNT[a, b]] // SUNSimplify
```

$$\frac{\delta ^{ab}}{2}$$

```mathematica
SUNTrace[SUNT[a, b, c]] // SUNSimplify
```

$$\frac{d^{abc}}{4}+\frac{1}{4} i f^{abc}$$

```mathematica
SUNTrace[SUNT[a, b, c, d]] // SUNSimplify[#, SUNTraceEvaluate -> True, SUNIndexNames -> {j}] &
```

$$\frac{1}{4} \delta ^{ad} \left(C_A-2 C_F\right) \delta ^{bc}-\frac{1}{4} \delta ^{ac} \left(C_A-2 C_F\right) \delta ^{bd}+\frac{1}{4} \delta ^{ab} \left(C_A-2 C_F\right) \delta ^{cd}-\frac{1}{8} i f^{adj} d^{bcj}+\frac{1}{8} i d^{adj} f^{bcj}+\frac{1}{8} d^{adj} d^{bcj}-\frac{1}{8} d^{bdj} d^{acj}+\frac{1}{8} d^{cdj} d^{abj}$$

```mathematica
SUNTrace[SUNT[a, b, c, a, b, c]] // SUNSimplify
```

$$\frac{1}{4} \left(C_A^2+1\right) C_F \left(C_A-2 C_F\right)$$