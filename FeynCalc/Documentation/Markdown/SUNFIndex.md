## SUNFIndex

`SUNFIndex[a]`  is an $SU(N)$ index in the fundamental representation. If the argument is an integer, `SUNFIndex[a]` turns into `ExplicitSUNFIndex[a]`.

### See also

[Overview](Extra/FeynCalc.md), [SUNIndex](SUNIndex.md).

### Examples

```mathematica
SUNFIndex[i]
```

$$i$$

```mathematica
SUNFIndex[i] // StandardForm

(*SUNFIndex[i]*)
```

```mathematica
SUNFIndex[2]
```

$$2$$

```mathematica
SUNFIndex[2] // StandardForm

(*ExplicitSUNFIndex[2]*)
```

```mathematica
SUNFDelta[i, j] // FCI // StandardForm

(*SUNFDelta[SUNFIndex[i], SUNFIndex[j]]*)
```