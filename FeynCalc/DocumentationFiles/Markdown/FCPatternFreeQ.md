## FCPatternFreeQ

`FCPatternFreeQ[{exp}]` yields `True` if `{exp}` does not contain any pattern objects, e.g. `Pattern`, `Blank`, `BlankSequence` and `BlankNullSequence`.

`FCPatternFreeQ[{exp},{h1,h2,...}]` checks that in addition to the pattern objects, no heads `h1, h2, ...` are present.

### See also

[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md).

### Examples

```mathematica
FCPatternFreeQ[{a}]
```

$$\text{True}$$

```mathematica
FCPatternFreeQ[{a_}]
```

$$\text{False}$$

```mathematica
FCPatternFreeQ[{g[x]}, {g}]
```

$$\text{False}$$