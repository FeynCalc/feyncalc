## SUNIndex

`SUNIndex[a]` is an $SU(N)$ index in the adjoint representation. If the argument is an integer, `SUNIndex[a]` turns into `ExplicitSUNIndex[a]`.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitSUNIndex](ExplicitSUNIndex.md), [SUNDelta](SUNDelta.md), [SUNF](SUNF.md).

### Examples

```mathematica
SUNIndex[i]
% // StandardForm
```

$$i$$

```
(*SUNIndex[i]*)
```

```mathematica
SUNIndex[2]
% // StandardForm
```

$$2$$

```
(*ExplicitSUNIndex[2]*)
```

```mathematica
SUNDelta[i, j] // FCI // StandardForm

(*SUNDelta[SUNIndex[i], SUNIndex[j]]*)
```
