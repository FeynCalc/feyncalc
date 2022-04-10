## SDF

`SDF[i, j]` denotes the $SU(N)$ Kronecker delta with color indices `i` and `j` in the fundamental representation. `SDF[i,j]` is transformed into `SUNFDelta[SUNFIndex[i],SUNFIndex[j]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [SUNFDelta](SUNFDelta.md).

### Examples

```mathematica
SDF[a, b]
```

$$\delta _{ab}$$

```mathematica
SDF[a, b] // FCI // StandardForm

(*SUNFDelta[SUNFIndex[a], SUNFIndex[b]]*)
```

```mathematica
SDF[a, b] // FCE // StandardForm

(*SDF[a, b]*)
```