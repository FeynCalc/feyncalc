## SUNSimplify

`SUNSimplify[exp]` simplifies color algebraic expressions involving color matrices with implicit (`SUNT`) or explicit fundamental indices (`SUNTF`) as well as structure constants (`SUND`, `SUNF`) and Kronecker deltas (`SD`, `SDF`).

If the option `Explicit` is set to `True` (default is `False`), the structure constants will be rewritten in terms of traces. However, since traces with 2 or 3 color matrices are by default converted back into structure constants, you must also set the option `SUNTraceEvaluate` to `False` (default is `Automatic`) in order to have unevaluated color traces in the output.

Many of the relations used in this routine (including derivations) can be found in [arXiv:1912.13302](https://arxiv.org/abs/1912.13302)

### See also

[Overview](Extra/FeynCalc.md), [SUNTrace](SUNTrace.md), [SUNFierz](SUNFierz.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNF](SUNF.md), [SUND](SUND.md), [SUNTraceEvaluate](SUNTraceEvaluate.md).

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

$$-\frac{1-N^2}{2 N}$$

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

$$f^{abc}$$

```mathematica
SUNSimplify[SUNF[a, b, c], Explicit -> True, SUNTraceEvaluate -> False]
```

$$2 i \;\text{tr}\left(T^a.T^c.T^b\right)-2 i \;\text{tr}\left(T^a.T^b.T^c\right)$$

```mathematica
SUNSimplify[SUND[a, b, c], Explicit -> True]
```

$$d^{abc}$$

```mathematica
SUNSimplify[SUND[a, b, c], Explicit -> True, SUNTraceEvaluate -> False]
```

$$2 \;\text{tr}\left(T^a.T^b.T^c\right)+2 \;\text{tr}\left(T^a.T^c.T^b\right)$$

```mathematica
SUNF[a, b, c] SUNT[c, b, a] 
 
SUNSimplify[%]
```

$$f^{abc} T^c.T^b.T^a$$

$$-\frac{1}{2} i C_A C_F$$

```mathematica
SUNF[a, b, e] SUNF[c, d, e] + SUNF[a, b, z] SUNF[c, d, z] 
 
SUNSimplify[%, SUNIndexNames -> {j}]
```

$$f^{abe} f^{cde}+f^{abz} f^{cdz}$$

$$2 f^{abj} f^{cdj}$$

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

```mathematica
SUNSimplify[SUNTrace[SUNT[i1, i2, i1, i2]], FCE -> True]
```

$$-\frac{C_F}{2}$$

`SUNSimplify` can also deal with chains of color matrices containing explicit fundamental indices (entered as `SUNTF`)

```mathematica
SUNTF[{a}, i, j] SUNTF[{a}, k, l] 
 
SUNSimplify[%]
```

$$T_{ij}^a T_{kl}^a$$

$$\frac{1}{2} \delta _{il} \delta _{jk}-\frac{1}{2} \left(C_A-2 C_F\right) \delta _{ij} \delta _{kl}$$

```mathematica
SUNTF[{b, a, c}, i, j] SUNTF[{d, a, e}, k, l] 
 
SUNSimplify[%]
```

$$\left(T^bT^aT^c\right){}_{ij} \left(T^dT^aT^e\right){}_{kl}$$

$$\frac{1}{2} \left(T^bT^e\right){}_{il} \left(T^dT^c\right){}_{kj}-\frac{1}{2} \left(C_A-2 C_F\right) \left(T^bT^c\right){}_{ij} \left(T^dT^e\right){}_{kl}$$

```mathematica
SUNTF[{a}, i, j] SUNTrace[SUNT[b, a, c]] 
 
SUNSimplify[%]

```mathematica

$$T_{ij}^a \;\text{tr}\left(T^b.T^a.T^c\right)$$

$$\frac{1}{2} \left(T^cT^b\right){}_{ij}-\frac{1}{4} \left(C_A-2 C_F\right) \delta ^{bc} \delta _{ij}$$