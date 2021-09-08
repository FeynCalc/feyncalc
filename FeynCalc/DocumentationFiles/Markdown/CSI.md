## CSI

`CSI[i]` can be used as input for 3-dimensional $\sigma ^i$ with 3-dimensional Cartesian index `i` and is transformed into `PauliSigma[CartesianIndex[i]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSI[i]
```

$$\overline{\sigma }^i$$

```mathematica
CSI[i, j] - CSI[j, i]
```

$$\overline{\sigma }^i.\overline{\sigma }^j-\overline{\sigma }^j.\overline{\sigma }^i$$

```mathematica
StandardForm[FCI[CSI[i]]]

(*PauliSigma[CartesianIndex[i]]*)
```

```mathematica
CSI[i, j, k, l]
```

$$\overline{\sigma }^i.\overline{\sigma }^j.\overline{\sigma }^k.\overline{\sigma }^l$$

```mathematica
StandardForm[CSI[i, j, k, l]]

(*CSI[i] . CSI[j] . CSI[k] . CSI[l]*)
```
