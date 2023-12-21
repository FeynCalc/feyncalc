## SUNIndex

`SUNIndex[a]` is an $SU(N)$ index in the adjoint representation. If the argument is an integer, `SUNIndex[a]` turns into `ExplicitSUNIndex[a]`.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitSUNIndex](ExplicitSUNIndex.md), [SUNDelta](SUNDelta.md), [SUNF](SUNF.md).

### Examples

```mathematica
SUNIndex[i]
```

$$i$$

```mathematica
SUNIndex[i] // StandardForm

(*SUNIndex[i]*)
```

```mathematica
SUNIndex[2]
```

$$2$$

```mathematica
SUNIndex[2] // StandardForm

(*ExplicitSUNIndex[2]*)
```

```mathematica
SUNDelta[i, j] // FCI // StandardForm

(*SUNDelta[SUNIndex[i], SUNIndex[j]]*)
```