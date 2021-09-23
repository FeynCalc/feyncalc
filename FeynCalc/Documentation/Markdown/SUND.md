## SUND

`SUND[a, b, c]` are the symmetric $SU(N)$ $d_{abc}$.

### See also

[Overview](Extra/FeynCalc.md), [SUNDelta](SUNDelta.md), [SUNF](SUNF.md), [SUNSimplify](SUNSimplify.md).

### Examples

```mathematica
SUND[a, b, c]
```

$$d^{abc}$$

```mathematica
SUND[a, b, c, Explicit -> True]
```

$$2 \left(\text{tr}(T^a.T^b.T^c)\right)+2 \left(\text{tr}(T^b.T^a.T^c)\right)$$

```mathematica
SUND[c, a, b]
```

$$d^{abc}$$

```mathematica
SUND[a, b, b]
```

$$d^{abb}$$

```mathematica
SUNSimplify[SUND[a, b, c] SUND[a, b, c]]
```

$$-2 \left(4-C_A^2\right) C_F$$

```mathematica
SUNSimplify[SUND[a, b, c] SUND[a, b, c], SUNNToCACF -> False] // Factor2
```

$$\frac{\left(1-N^2\right) \left(4-N^2\right)}{N}$$

```mathematica
SUNSimplify[SUND[a, b, c] SUND[e, b, c], SUNNToCACF -> False] // Factor2
```

$$-\frac{\left(4-N^2\right) \delta ^{ae}}{N}$$

```mathematica
SUND[a, b, c] // StandardForm

(*SUND[a, b, c]*)
```

```mathematica
SUND[a, b, c] // FCI // StandardForm

(*SUND[SUNIndex[a], SUNIndex[b], SUNIndex[c]]*)
```

```mathematica
SUND[a, b, c] // FCI // FCE // StandardForm

(*SUND[a, b, c]*)
```
