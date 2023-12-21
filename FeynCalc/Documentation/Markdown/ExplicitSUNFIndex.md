## ExplicitSUNFIndex

`ExplicitSUNFIndex[ind]` is a specific $SU(N)$ index in the fundamental representation, i.e. `ind` is an integer.

### See also

[Overview](Extra/FeynCalc.md), [SUNIndex](SUNIndex.md), [SUNFIndex](SUNFIndex.md).

### Examples

```mathematica
ExplicitSUNFIndex[1]
```

$$1$$

```mathematica
SUNTF[a, 1, 2]
```

$$T_{12}^a$$

```mathematica
SUNTF[a, 1, 2] // FCI // StandardForm

(*SUNTF[{SUNIndex[a]}, ExplicitSUNFIndex[1], ExplicitSUNFIndex[2]]*)
```