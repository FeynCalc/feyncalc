## CGA

`CGA[i]` can be used as input for $\gamma^i$ in 4 dimensions, where `i` is a Cartesian index, and is transformed into `DiracGamma[CartesianIndex[i]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [GA](GA.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
CGA[i]
```

$$\overline{\gamma }^i$$

```mathematica
CGA[i, j] - CGA[j, i]
```

$$\overline{\gamma }^i.\overline{\gamma }^j-\overline{\gamma }^j.\overline{\gamma }^i$$

```mathematica
StandardForm[FCI[CGA[i]]]

(*DiracGamma[CartesianIndex[i]]*)
```

```mathematica
CGA[i, j, k, l]
```

$$\overline{\gamma }^i.\overline{\gamma }^j.\overline{\gamma }^k.\overline{\gamma }^l$$

```mathematica
StandardForm[CGA[i, j, k, l]]

(*CGA[i] . CGA[j] . CGA[k] . CGA[l]*)
```

```mathematica
DiracSimplify[DiracTrace[CGA[i, j, k, l]]]
```

$$4 \bar{\delta }^{il} \bar{\delta }^{jk}-4 \bar{\delta }^{ik} \bar{\delta }^{jl}+4 \bar{\delta }^{ij} \bar{\delta }^{kl}$$

```mathematica
CGA[i] . (CGS[p] + m) . CGA[j]
```

$$\overline{\gamma }^i.\left(\overline{\gamma }\cdot \overline{p}+m\right).\overline{\gamma }^j$$
