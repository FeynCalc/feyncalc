`Polarization[k]` is the head of a polarization momentum with (incoming) momentum `k`.

A slashed polarization vector ($\varepsilon_{\mu}(k) \gamma^\mu$ has to be entered as `GS[Polarization[k]]`.

The internal representation for a polarization vector corresponding to a boson with four momentum k is: `Momentum[Polarization[k, I ]]`. Unless the option `Transversality` is set to `True`, all polarization vectors are not transverse by default.

`Polarization[k,-I]` denotes the complex conjugate polarization.

Polarization is also an option of various functions related to the operator product expansion. The setting `0` denotes the unpolarized and `1` the polarized case.

### See also

[PolarizationVector](PolarizationVector), [PolarizationSum](PolarizationSum), [DoPolarizationSums](DoPolarizationSums).

### Examples

```mathematica
Polarization[k]
```

$$\text{Polarization}(k,i)$$

```mathematica
Polarization[k] // ComplexConjugate
```

$$\text{Polarization}(k,-i)$$

```mathematica
GS[Polarization[k]]
```

$$\bar{\gamma }\cdot \bar{\varepsilon }(k)$$

```mathematica
GS[Polarization[k]] // StandardForm

(*GS[Polarization[k, I]]*)
```

```mathematica
Pair[Momentum[k], Momentum[Polarization[k, I]]]
```

$$\overline{k}\cdot \bar{\varepsilon }(k)$$