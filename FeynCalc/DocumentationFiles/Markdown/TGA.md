## TGA

`TGA[]`  can be used as input for $\gamma^0$ in $4$ dimensions and is transformed into `DiracGamma[ExplicitLorentzIndex[0]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [GA](GA.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
TGA[]
```

$$\bar{\gamma }^0$$

```mathematica
TGA[] // FCI // StandardForm

(*DiracGamma[ExplicitLorentzIndex[0]]*)
```

```mathematica
TGA[] . TGA[] // DiracSimplify
```

$$1$$
