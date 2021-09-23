## CGAD

`CGAD[i]` can be used as input for $\gamma ^i$ in $D$ dimensions, where `i` is a Cartesian index, and is transformed into `DiracGamma[CartesianIndex[i,D-1],D]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [GAD](GAD.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
CGAD[i]
```

$$\gamma ^i$$

```mathematica
CGAD[i, j] - CGAD[j, i]
```

$$\gamma ^i.\gamma ^j-\gamma ^j.\gamma ^i$$

```mathematica
StandardForm[FCI[CGAD[i]]]

(*DiracGamma[CartesianIndex[i, -1 + D], D]*)
```

```mathematica
CGAD[i, j, k, l]
```

$$\gamma ^i.\gamma ^j.\gamma ^k.\gamma ^l$$

```mathematica
StandardForm[CGAD[i, j, k, l]]

(*CGAD[i] . CGAD[j] . CGAD[k] . CGAD[l]*)
```

```mathematica
DiracSimplify[DiracTrace[CGAD[i, j, k, l]]]
```

$$4 \delta ^{il} \delta ^{jk}-4 \delta ^{ik} \delta ^{jl}+4 \delta ^{ij} \delta ^{kl}$$

```mathematica
CGAD[i] . (CGSD[p] + m) . CGAD[j]
```

$$\gamma ^i.(m+\gamma \cdot p).\gamma ^j$$
