##  SUNTF 

SUNTF[{a}, i, j] is the SU(N) $T^a$ generator in the fundamental representation. The fundamental indices are explicit..

###  Examples 

```mathematica
SUNTF[a, i, j] 
 
SUNTF[{a, b}, i, j]
```

$$T_{ij}^a$$

$$\left(T^aT^b\right){}_{ij}$$

SUNTF are c-numbers, hence they are commutative objects and do not require a dot

```mathematica
SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, k] 
 
SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, k] // SUNFSimplify
```

$$\left(T^aT^b\right){}_{ij} \left(T^cT^d\right){}_{jk}$$

$$\left(T^aT^bT^cT^d\right){}_{ik}$$

A chain with closed indices is automatically converted into a trace

```mathematica
SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, i] // SUNFSimplify
```

$$\text{tr}(T^a.T^b.T^c.T^d)$$

SUNFSimplify is a dedicated function to deal with SUNTFs. However, SUNSimplify will also call SUNFSimplify when it detects SUNTFs in the input

```mathematica
SUNDelta[a, b] SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, i] // SUNSimplify 
 
SUNTF[{a, b}, i, j] // FCI // StandardForm
```

$$\frac{1}{2} C_F \delta ^{cd}$$

```
(*SUNTF[{SUNIndex[a], SUNIndex[b]}, SUNFIndex[i], SUNFIndex[j]]*)
```