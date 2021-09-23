`Li3` is an abbreviation for the trilogarithm function, i.e. `Li3 = PolyLog[3, #]&`.

### See also

[Overview](Extra/FeynCalc.md), [Li2](Li2.md).

### Examples

```mathematica
Li3[x]
```

$$\text{Li}_3(x)$$

```mathematica
Li3 // StandardForm

(*PolyLog[3, #1] &*)
```

```mathematica
D[Li3[x], x]
```

$$\frac{\text{Li}_2(x)}{x}$$

```mathematica
Integrate[Li3[x]/x, x]
```

$$\text{Li}_4(x)$$
