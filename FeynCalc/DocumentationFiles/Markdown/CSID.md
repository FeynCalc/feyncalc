## CSID

`CSID[i]` can be used as input for $D-1$-dimensional $\sigma^i$ with $D-1$-dimensional Cartesian index `i` and is transformed into `PauliSigma[CartesianIndex[i,D-1],D-1]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSID[i]
```

$$\sigma ^i$$

```mathematica
CSID[i, j] - CSID[j, i]
```

$$\sigma ^i.\sigma ^j-\sigma ^j.\sigma ^i$$

```mathematica
StandardForm[FCI[CSID[i]]]

(*PauliSigma[CartesianIndex[i, -1 + D], -1 + D]*)
```

```mathematica
CSID[i, j, k, l]
```

$$\sigma ^i.\sigma ^j.\sigma ^k.\sigma ^l$$

```mathematica
StandardForm[CSID[i, j, k, l]]

(*CSID[i] . CSID[j] . CSID[k] . CSID[l]*)
```
