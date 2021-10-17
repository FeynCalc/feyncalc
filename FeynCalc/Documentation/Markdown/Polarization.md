## Polarization

`Polarization[k]` is the head of a polarization momentum with momentum `k`.

A slashed polarization vector ($\varepsilon_{\mu}(k) \gamma^\mu)$ has to be entered as `GS[Polarization[k]]`.

Unless the option `Transversality` is set to `True`, all polarization vectors are not transverse by default.

The internal representation for a polarization vector corresponding to a boson with four momentum $k$ is: `Momentum[Polarization[k, I ]]`.

`Polarization[k,-I]` denotes the complex conjugate polarization.

Polarization is also an option of various functions related to the operator product expansion. The setting `0` denotes the unpolarized and `1` the polarized case.

`Polarization` may appear only inside `Momentum`. Outside of `Momentum` it is meaningless in FeynCalc.

The imaginary unit in the second argument of `Polarization` is used to distinguish between incoming and outgoing polarization vectors.

- `Pair[Momentum[k], Momentum[Polarization[k, I]]]` corresponds to $\varepsilon^{\mu}(k)$, i.e. an ingoing polarization vector

- `Pair[Momentum[k], Momentum[Polarization[k, -I]]]` corresponds to $\varepsilon^{\ast \mu}(k)$, i.e. an outgoing polarization vector

### See also

[Overview](Extra/FeynCalc.md), [PolarizationVector](PolarizationVector.md), [PolarizationSum](PolarizationSum.md), [DoPolarizationSums](DoPolarizationSums.md).

### Examples

```mathematica
Polarization[k]
```

$$\text{Polarization}(k)$$

```mathematica
Polarization[k] // ComplexConjugate
```

$$\text{ComplexConjugate}(\text{Polarization}(k))$$

```mathematica
GS[Polarization[k]]
```

$$\text{GS}(\text{Polarization}(k))$$

```mathematica
GS[Polarization[k]] // StandardForm

(*GS[Polarization[k]]*)
```

```mathematica
Pair[Momentum[k], Momentum[Polarization[k, I]]]
```

$$\text{Pair}(\text{Momentum}(k),\text{Momentum}(\text{Polarization}(k,i)))$$