## SquareAmplitude

`SquareAmplitude[m1, m2]` multiplies the amplitudes from the list `m1` with their complex conjugate from the list `m2` to obtain the list of products $m1_i m2_j$. This function can be useful when exporting amplitudes obtained with FeynCalc to FORM.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
Clear[a1, a2, a3, b1, b2, b3]
```

```mathematica
SquareAmplitude[{a1, a2, a3}, {b1, b2, b3}]
```

$$\text{SquareAmplitude}(\{\text{a1},\text{a2},\text{a3}\},\{\text{b1},\text{b2},\text{b3}\})$$

```mathematica
SquareAmplitude[{a1, a2, a3}, {b1, b2, b3}, List -> False]
```

$$\text{SquareAmplitude}(\{\text{a1},\text{a2},\text{a3}\},\{\text{b1},\text{b2},\text{b3}\},\text{List}\to \;\text{False})$$

When the option `Real` is set to `True`, the amplitudes are assumed to have no imaginary part

```mathematica
SquareAmplitude[{a1, a2, a3}, {b1, b2, b3}, Real -> True, List -> False]
```

$$\text{SquareAmplitude}(\{\text{a1},\text{a2},\text{a3}\},\{\text{b1},\text{b2},\text{b3}\},\text{Real}\to \;\text{True},\text{List}\to \;\text{False})$$

The option `Indexed` allows us to attach a marker to each contribution

```mathematica
SquareAmplitude[{a1, a2, a3}, {b1, b2, b3}, Real -> True, List -> False, Indexed -> mark]
```

$$\text{SquareAmplitude}(\{\text{a1},\text{a2},\text{a3}\},\{\text{b1},\text{b2},\text{b3}\},\text{Real}\to \;\text{True},\text{List}\to \;\text{False},\text{Indexed}\to \;\text{mark})$$
