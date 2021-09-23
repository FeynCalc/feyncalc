## SUNTF

`SUNTF[{a}, i, j]` is the $SU(N)$ $T^a$ generator in the fundamental representation. The fundamental indices are explicit.

### See also

[Overview](Extra/FeynCalc.md), [SUNFindex](SUNFindex.md), [SUNT](SUNT.md), [SUNSimplify](SUNSimplify.md).

### Examples

```mathematica
SUNTF[a, i, j]
```

$$T_{ij}^a$$

```mathematica
SUNTF[{a, b}, i, j]
```

$$\left(T^aT^b\right){}_{ij}$$

`SUNTF` are $c$-numbers, hence they are commutative objects and do not require a dot

```mathematica
SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, k]
```

$$\left(T^aT^b\right){}_{ij} \left(T^cT^d\right){}_{jk}$$

```mathematica
SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, k] // SUNFSimplify
```

$$\left(T^aT^bT^cT^d\right){}_{ik}$$

A chain with closed indices is automatically converted into a trace

```mathematica
SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, i] // SUNFSimplify
```

$$\text{tr}(T^a.T^b.T^c.T^d)$$

`SUNFSimplify` is a dedicated function to deal with `SUNTF`s. However, `SUNSimplify` will also call `SUNFSimplify` when it detects `SUNTF`objects in the input

```mathematica
SUNDelta[a, b] SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, i] // SUNSimplify
```

$$\frac{1}{2} C_F \delta ^{cd}$$

```mathematica
SUNTF[{a, b}, i, j] // FCI // StandardForm

(*SUNTF[{SUNIndex[a], SUNIndex[b]}, SUNFIndex[i], SUNFIndex[j]]*)
```
