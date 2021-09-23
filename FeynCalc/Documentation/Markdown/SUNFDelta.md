## SUNFDelta

`SUNFDelta[a, b]` is the Kronecker-delta for $SU(N)$ with color indices `a` and `b` in the fundamental representation.

### See also

[Overview](Extra/FeynCalc.md), [SUNDelta](SUNDelta.md).

### Examples

```mathematica
SUNFDelta[SUNFIndex[a], SUNFIndex[b]]
```

$$\delta _{ab}$$

```mathematica
SUNFDelta[SUNFIndex[a], SUNFIndex[b]] SUNFDelta[SUNFIndex[b], SUNFIndex[c]]
% // SUNSimplify
```

$$\delta _{ab} \delta _{bc}$$

$$\delta _{ac}$$

```mathematica
SUNFDelta[SUNFIndex[a], SUNFIndex[b]]^2
% // SUNSimplify
```

$$\delta _{ab}^2$$

$$C_A$$

```mathematica
SUNFDelta[SUNFIndex[a], SUNFIndex[b]] // StandardForm

(*SUNFDelta[SUNFIndex[a], SUNFIndex[b]]*)
```

```mathematica
SUNFDelta[SUNFIndex[a], SUNFIndex[b]] // FCI // FCE // StandardForm

(*SDF[a, b]*)
```

```mathematica
SDF[a, b] // FCI // StandardForm

(*SUNFDelta[SUNFIndex[a], SUNFIndex[b]]*)
```

The arguments of `SUNFDelta` may also represent explicit integer indices via the head `ExplictiSUNFIndex`. The difference is that `SUNSimplify` and `SUNFSimplify` will only sum over symbolic indices.

```mathematica
SUNFDelta[SUNFIndex[a], ExplicitSUNFIndex[2]] SUNFDelta[SUNFIndex[a], SUNFIndex[b]] SUNFDelta[SUNFIndex[c], ExplicitSUNFIndex[2]] // SUNFSimplify
% // StandardForm
```

$$\delta _{2b} \delta _{2c}$$

```
(*SUNFDelta[ExplicitSUNFIndex[2], SUNFIndex[b]] SUNFDelta[ExplicitSUNFIndex[2], SUNFIndex[c]]*)
```

```mathematica
SDF[1, 2] // FCI // StandardForm

(*SUNFDelta[ExplicitSUNFIndex[1], ExplicitSUNFIndex[2]]*)
```
