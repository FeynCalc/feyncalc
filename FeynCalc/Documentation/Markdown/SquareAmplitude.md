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

$$\{\text{a1} \;\text{b1},\text{a1} \;\text{b2},\text{a1} \;\text{b3},\text{a2} \;\text{b1},\text{a2} \;\text{b2},\text{a2} \;\text{b3},\text{a3} \;\text{b1},\text{a3} \;\text{b2},\text{a3} \;\text{b3}\}$$

```mathematica
SquareAmplitude[{a1, a2, a3}, {b1, b2, b3}, List -> False]
```

$$\text{a1} \;\text{b1}+\text{a1} \;\text{b2}+\text{a1} \;\text{b3}+\text{a2} \;\text{b1}+\text{a2} \;\text{b2}+\text{a2} \;\text{b3}+\text{a3} \;\text{b1}+\text{a3} \;\text{b2}+\text{a3} \;\text{b3}$$

When the option `Real` is set to `True`, the amplitudes are assumed to have no imaginary part

```mathematica
SquareAmplitude[{a1, a2, a3}, {b1, b2, b3}, Real -> True, List -> False]
```

$$\text{a1} \;\text{b1}+2 \;\text{a2} \;\text{b1}+\text{a2} \;\text{b2}+2 \;\text{a3} \;\text{b1}+2 \;\text{a3} \;\text{b2}+\text{a3} \;\text{b3}$$

The option `Indexed` allows us to attach a marker to each contribution

```mathematica
SquareAmplitude[{a1, a2, a3}, {b1, b2, b3}, Real -> True, List -> False, Indexed -> mark]
```

$$\text{a1} \;\text{b1} \;\text{mark}(1,1)+2 \;\text{a2} \;\text{b1} \;\text{mark}(2,1)+\text{a2} \;\text{b2} \;\text{mark}(2,2)+2 \;\text{a3} \;\text{b1} \;\text{mark}(3,1)+2 \;\text{a3} \;\text{b2} \;\text{mark}(3,2)+\text{a3} \;\text{b3} \;\text{mark}(3,3)$$