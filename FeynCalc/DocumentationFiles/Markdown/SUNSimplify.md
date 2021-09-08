## SUNSimplify

`SUNSimplify[exp]` simplifies products of `SUNT` and `SUNTF` matrices in the expression.

### See also

[Overview](Extra/FeynCalc.md), [SUNTrace](SUNTrace.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNF](SUNF.md), [SUND](SUND.md).

### Examples

```mathematica
SUNDelta[a, b] SUNDelta[b, c]
SUNSimplify[%]
```

$$\delta ^{ab} \delta ^{bc}$$

$$\delta ^{ac}$$

```mathematica
SUNT[a] . SUNT[a]
SUNSimplify[%]
```

$$T^a.T^a$$

$$C_F$$

```mathematica
SUNSimplify[SUNT[a] . SUNT[a], SUNNToCACF -> False]
```

$$\frac{N^2-1}{2 N}$$

```mathematica
SUNF[a, r, s] SUNF[b, r, s]
SUNSimplify[%]
```

$$f^{ars} f^{brs}$$

$$C_A \delta ^{ab}$$

```mathematica
SUNF[a, b, c]  SUNF[a, b, c]
SUNSimplify[%]
```

$$\left(f^{abc}\right)^2$$

$$2 C_A^2 C_F$$

```mathematica
SUNF[a, b, c] SUNF[d, b, c]
SUNSimplify[%]
```

$$f^{abc} f^{dbc}$$

$$C_A \delta ^{ad}$$

```mathematica
SUNF[a, b, c] SUND[d, b, c]
SUNSimplify[%, Explicit -> True]
```

$$d^{bcd} f^{abc}$$

$$0$$

```mathematica
SUND[a, b, c] SUND[a, b, c]
SUNSimplify[%, SUNNToCACF -> False] // Factor2
```

$$\left(d^{abc}\right)^2$$

$$\frac{\left(1-N^2\right) \left(4-N^2\right)}{N}$$

```mathematica
SUNSimplify[SUND[a, b, c] SUND[e, b, c], SUNNToCACF -> False] // Simplify
```

$$\frac{\left(N^2-4\right) \delta ^{ae}}{N}$$

```mathematica
SUNSimplify[SUNF[a, b, c], Explicit -> True]
```

$$-2 i \left(\text{tr}(T^a.T^b.T^c)-\text{tr}(T^b.T^a.T^c)\right)$$

```mathematica
SUNSimplify[SUND[a, b, c], Explicit -> True]
```

$$2 \left(\text{tr}(T^a.T^b.T^c)+\text{tr}(T^b.T^a.T^c)\right)$$

```mathematica
SUNF[a, b, c] SUNT[c, b, a]
SUNSimplify[%]
```

$$f^{abc} T^c.T^b.T^a$$

$$-\frac{1}{2} i C_A C_F$$

```mathematica
SUNF[a, b, e] SUNF[c, d, e] + SUNF[a, b, z] SUNF[c, d, z]
SUNSimplify[%, Explicit -> False]
SUNSimplify[%, Explicit -> False, SUNIndexRename -> False]
```

$$f^{abe} f^{cde}+f^{abz} f^{cdz}$$

$$2 f^{abe} f^{cde}$$

$$2 f^{abe} f^{cde}$$

```mathematica
SUNSimplify[1 - SD[i, i]]
```

$$2-C_A^2$$

```mathematica
SUNSimplify[SUNF[a, b, c] SUND[d, b, c]]
```

$$0$$

```mathematica
SUNSimplify[SUNF[a, b, c] SUND[a, b, d]]
```

$$0$$

```mathematica
SUNSimplify[SUNF[a, b, c] SUND[a, d, c]]
```

$$0$$

```mathematica
SUNSimplify[SUND[a, b, c] SUND[d, b, c]]
```

$$-\left(\left(4-C_A^2\right) \delta ^{ad} \left(C_A-2 C_F\right)\right)$$
