`Li2` is an abbreviation for the dilogarithm function, i.e. `Li2 = PolyLog[2, #]&`.

### See also

[Li3](Li3), [Li4](Li4).

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