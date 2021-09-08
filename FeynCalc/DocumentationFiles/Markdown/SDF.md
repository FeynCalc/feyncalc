## SDF

`SDF[i, j]` denotes the $SU(N)$ Kronecker delta with color indices `i` and `j` in the fundamental representation. `SDF[i,j]` is transformed into `SUNFDelta[SUNFIndex[i],SUNFIndex[j]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [SUNFDelta](SUNFDelta.md).

### Examples

```mathematica
SDF[a, b]
% // FCI // StandardForm
% // FCE // StandardForm
```

$$\delta _{ab}$$

```
(*SUNFDelta[SUNFIndex[a], SUNFIndex[b]]*)

(*SDF[a, b]*)
```
