## Color algebra

### See also

[Overview](Extra/FeynCalc.md).

### Notation for colored objects

FeynCalc objects relevant for the color algebra are

```mathematica
SUNT[a]
```

$$T^a$$

```mathematica
SUNF[a, b, c]
```

$$f^{abc}$$

```mathematica
SUND[a, b, c]
```

$$d^{abc}$$

```mathematica
SUNDelta[a, b]
```

$$\delta ^{ab}$$

```mathematica
SUNN
```

$$N$$

```mathematica
CA
```

$$C_A$$

```mathematica
CF
```

$$C_F$$

There are two main functions to deal with colored objects: `SUNSimplify` and `SUNTrace`

```mathematica
SUNT[a, a]
SUNSimplify[%]
```

$$T^a.T^a$$

$$C_F$$

```mathematica
SUNT[a, b, a, b]
SUNSimplify[%]
```

$$T^a.T^b.T^a.T^b$$

$$-\frac{1}{2} C_F \left(C_A-2 C_F\right)$$

```mathematica
SUNT[b, d, a, b, d]
SUNSimplify[%]
```

$$T^b.T^d.T^a.T^b.T^d$$

$$\frac{T^a \left(C_A^2+1\right)}{4 C_A^2}$$

The color factors $C_A$ and $C_F$ are reconstructed from $N_c$ using heuristics. The reconstruction can be disabled by setting the option `SUNNToCACF` to `False`

```mathematica
SUNSimplify[SUNT[b, d, a, b, d], SUNNToCACF -> False]
```

$$\frac{\left(N^2+1\right) T^a}{4 N^2}$$

The color traces are not evaluated by default. The evaluation can be forced either by applying `SUNSimplify` or setting the option `SUNTraceEvaluate` to `True`

```mathematica
SUNTrace[SUNT[a, b]]
```

$$\text{tr}\left(T^a.T^b\right)$$

```mathematica
SUNTrace[SUNT[a, b, b, a]]
```

$$\text{tr}\left(T^a.T^b.T^b.T^a\right)$$

```mathematica
SUNTrace[SUNT[a, b]] // SUNSimplify
```

$$\frac{\delta ^{ab}}{2}$$

```mathematica
SUNTrace[SUNT[a, b, b, a]] // SUNSimplify
```

$$C_A C_F^2$$

```mathematica
SUNTrace[SUNT[a, b], SUNTraceEvaluate -> True]
```

$$\frac{\delta ^{ab}}{2}$$

Use `SUNTF` to get color matrices with explicit fundamental indices

```mathematica
SUNTF[{a, b, c}, i, j] SUNTrace[SUNT[b, a]]
% // SUNSimplify
```

$$\text{tr}\left(T^b.T^a\right) \left(T^aT^bT^c\right){}_{ij}$$

$$\frac{1}{2} C_F T_{ij}^c$$

Color traces with more than 3 matrices are not evaluated by default (assuming that no other simplifications are possible). The evaluation can be forced using the option `SUNTraceEvaluate` set to `True`

```mathematica
SUNTrace[SUNT[a, b, c, d]] // SUNSimplify[#, SUNTraceEvaluate -> True] &
```

$$\frac{1}{4} \delta ^{ad} \left(C_A-2 C_F\right) \delta ^{bc}-\frac{1}{4} \delta ^{ac} \left(C_A-2 C_F\right) \delta ^{bd}+\frac{1}{4} \delta ^{ab} \left(C_A-2 C_F\right) \delta ^{cd}-\frac{1}{8} i f^{ad\text{FCGV}(\text{sun941})} d^{bc\text{FCGV}(\text{sun941})}+\frac{1}{8} i d^{ad\text{FCGV}(\text{sun941})} f^{bc\text{FCGV}(\text{sun941})}+\frac{1}{8} d^{ad\text{FCGV}(\text{sun941})} d^{bc\text{FCGV}(\text{sun941})}-\frac{1}{8} d^{bd\text{FCGV}(\text{sun941})} d^{ac\text{FCGV}(\text{sun941})}+\frac{1}{8} d^{cd\text{FCGV}(\text{sun941})} d^{ab\text{FCGV}(\text{sun941})}$$

One can automatically rename dummy indices using the `SUNIndexNames` option

```mathematica
SUNTrace[SUNT[a, b, c, d]] // SUNSimplify[#, SUNTraceEvaluate -> True, SUNIndexNames -> {j}] &
```

$$\frac{1}{4} \delta ^{ad} \left(C_A-2 C_F\right) \delta ^{bc}-\frac{1}{4} \delta ^{ac} \left(C_A-2 C_F\right) \delta ^{bd}+\frac{1}{4} \delta ^{ab} \left(C_A-2 C_F\right) \delta ^{cd}-\frac{1}{8} i f^{adj} d^{bcj}+\frac{1}{8} i d^{adj} f^{bcj}+\frac{1}{8} d^{adj} d^{bcj}-\frac{1}{8} d^{bdj} d^{acj}+\frac{1}{8} d^{cdj} d^{abj}$$