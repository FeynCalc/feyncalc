`Li2` is an abbreviation for the dilogarithm function, i.e. `Li2 = PolyLog[2, #]&`.

### See also

[Overview](Extra/FeynCalc.md), [Li3](Li3.md), [Li4](Li4.md).

### Examples

```mathematica
Li2[x]
```

$$\text{Li}_2(x)$$

```mathematica
Li2 // StandardForm

(*PolyLog[2, #1] &*)
```

```mathematica
Integrate[-Log[1 - x]/x, x]
```

$$\text{Li}_2(x)$$
