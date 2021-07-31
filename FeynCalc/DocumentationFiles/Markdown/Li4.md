`Li4` is an abbreviation for the weight 4 polylogarithm function, i.e. `Li4 = PolyLog[4, #]&`.

### See also

[Li2](Li2), [Li3](Li3), [SimplifyPolyLog](SimplifyPolyLog).

### Examples

```mathematica
Li4[x]
```

$$\text{Li}_4(x)$$

```mathematica
Li4 // StandardForm

(*PolyLog[4, #1] &*)
```

```mathematica
D[Li4[x], x]
```

$$\frac{\text{Li}_3(x)}{x}$$

```mathematica
Integrate[Li3[x]/x, x]
```

$$\text{Li}_4(x)$$