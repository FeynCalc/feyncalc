## CGAE

`CGAE[i]` can be used as input for $\gamma ^i$ in $D-4$ dimensions, where `i` is a Cartesian index, and is transformed into `DiracGamma[CartesianIndex[i,D-4],D-4]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [GAE](GAE.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
CGAE[i]
```

$$\hat{\gamma }^i$$

```mathematica
CGAE[i, j] - CGAE[j, i]
```

$$\hat{\gamma }^i.\hat{\gamma }^j-\hat{\gamma }^j.\hat{\gamma }^i$$

```mathematica
StandardForm[FCI[CGAE[i]]]

(*DiracGamma[CartesianIndex[i, -4 + D], -4 + D]*)
```

```mathematica
CGAE[i, j, k, l]
```

$$\hat{\gamma }^i.\hat{\gamma }^j.\hat{\gamma }^k.\hat{\gamma }^l$$

```mathematica
StandardForm[CGAE[i, j, k, l]]

(*CGAE[i] . CGAE[j] . CGAE[k] . CGAE[l]*)
```
