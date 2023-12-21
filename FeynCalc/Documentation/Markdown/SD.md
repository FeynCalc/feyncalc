## SD

`SD[i, j]` denotes the $SU(N)$ Kronecker delta with color indices `i` and `j` in the adjoint representation.

`SD[i,j]` is transformed into `SUNDelta[SUNIndex[i],SUNIndex[j]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [SUNDelta](SUNDelta.md).

### Examples

```mathematica
SD[a, b]
```

$$\delta ^{ab}$$

```mathematica
SD[a, b] // FCI // StandardForm

(*SUNDelta[SUNIndex[a], SUNIndex[b]]*)
```

```mathematica
SD[a, b] // FCE // StandardForm

(*SD[a, b]*)
```