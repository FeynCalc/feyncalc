## CSIE

`CSIE[i]` can be used as input for $D-4$-dimensional $\sigma ^i$ with $D-4$-dimensional Cartesian index `i` and is transformed into `PauliSigma[CartesianIndex[i,D-4],D-4]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSIE[i]
```

$$\hat{\sigma }^i$$

```mathematica
CSIE[i, j] - CSIE[j, i]
```

$$\hat{\sigma }^i.\hat{\sigma }^j-\hat{\sigma }^j.\hat{\sigma }^i$$

```mathematica
StandardForm[FCI[CSIE[i]]]

(*PauliSigma[CartesianIndex[i, -4 + D], -4 + D]*)
```

```mathematica
CSIE[i, j, k, l]
```

$$\hat{\sigma }^i.\hat{\sigma }^j.\hat{\sigma }^k.\hat{\sigma }^l$$

```mathematica
StandardForm[CSIE[i, j, k, l]]

(*CSIE[i] . CSIE[j] . CSIE[k] . CSIE[l]*)
```
