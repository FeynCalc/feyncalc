## SUNFSimplify

`SUNFSimplify[exp]` is an auxiliary function that simplifies expressions containing $\text{SU}(N)$ indices in the fundamental representation. The simplifications performed by `SUNFSimplify` are mostly limited to the contractions of the fundamental indices. The function is by far not as powerful as `SUNSimplify`.

### See also

[Overview](Extra/FeynCalc.md), [SUNTF](SUNTF.md), [SUNFDelta](SUNFDelta.md).

### Examples

```mathematica
SDF[a, a]
SUNFSimplify[%]
```

$$\delta _{aa}$$

$$C_A$$

```mathematica
SUNFSimplify[SDF[a, a], SUNNToCACF -> False]
```

$$N$$

```mathematica
SDF[a, b] SDF[b, d]
SUNFSimplify[%]
```

$$\delta _{ab} \delta _{bd}$$

$$\delta _{ad}$$

```mathematica
SDF[a, b] SUNTF[i, a, d] SUNTF[j, d, c]
SUNFSimplify[%]
```

$$\delta _{ab} T_{ad}^i T_{dc}^j$$

$$\left(T^iT^j\right){}_{bc}$$

```mathematica
SDF[a, b] (SUNTF[i, a, d] SUNTF[j, d, b] + SD[i, j] SUNTF[i, a, d] SUNTF[i, d, b])
SUNFSimplify[%, SUNNToCACF -> False]
```

$$\delta _{ab} \left(\delta ^{ij} T_{ad}^i T_{db}^i+T_{ad}^i T_{db}^j\right)$$

$$\frac{\delta ^{ij}}{2}+\frac{1}{2} \left(N^2-1\right) \delta ^{ij}$$
