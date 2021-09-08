## SUNDelta

`SUNDelta[a, b]`  is the Kronecker-delta for $SU(N)$ with color indices `a` and `b` in the adjoint representation.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitSUNIndex](ExplicitSUNIndex.md), [SD](SD.md), [SUNF](SUNF.md), [SUNIndex](SUNIndex.md), [SUNSimplify](SUNSimplify.md), [Trick](Trick.md).

### Examples

```mathematica
SUNDelta[SUNIndex[a], SUNIndex[b]]
```

$$\delta ^{ab}$$

```mathematica
SUNDelta[SUNIndex[a], SUNIndex[b]] SUNDelta[SUNIndex[b], SUNIndex[c]]
SUNSimplify[%]
```

$$\delta ^{ab} \delta ^{bc}$$

$$\delta ^{ac}$$

```mathematica
SUNDelta[SUNIndex[a], SUNIndex[b]]
% // StandardForm
```

$$\delta ^{ab}$$

```
(*SUNDelta[SUNIndex[a], SUNIndex[b]]*)
```

```mathematica
SUNDelta[SUNIndex[a], SUNIndex[b]] // FCI // FCE // StandardForm

(*SD[a, b]*)
```

```mathematica
SD[a, b] // FCI // StandardForm

(*SUNDelta[SUNIndex[a], SUNIndex[b]]*)
```

The arguments of `SUNDelta` may also represent explicit integer indices via the head `ExplictiSUNIndex`. The difference is that `SUNSimplify` will only sum over symbolic indices.

```mathematica
SUNDelta[SUNIndex[a], ExplicitSUNIndex[2]] SUNDelta[SUNIndex[a], SUNIndex[b]]*
   SUNDelta[SUNIndex[c], ExplicitSUNIndex[2]] // SUNSimplify
```

$$\delta ^{2b} \delta ^{2c}$$

```mathematica
% // StandardForm

(*SUNDelta[ExplicitSUNIndex[2], SUNIndex[b]] SUNDelta[ExplicitSUNIndex[2], SUNIndex[c]]*)
```

```mathematica
SD[1, 2] // FCI // StandardForm

(*SUNDelta[ExplicitSUNIndex[1], ExplicitSUNIndex[2]]*)
```
