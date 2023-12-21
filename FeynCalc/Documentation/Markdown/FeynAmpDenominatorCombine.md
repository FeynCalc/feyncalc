## FeynAmpDenominatorCombine

`FeynAmpDenominatorCombine[expr]` expands expr with respect to `FeynAmpDenominator` and combines products of `FeynAmpDenominator` in expr into one `FeynAmpDenominator`.

### See also

[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorSplit](FeynAmpDenominatorSplit.md).

### Examples

```mathematica
FAD[q] FAD[q - p] 
 
ex = FeynAmpDenominatorCombine[%]
```

$$\frac{1}{q^2 (q-p)^2}$$

$$\frac{1}{q^2.(q-p)^2}$$

```mathematica
ex // FCE // StandardForm

(*FAD[q, -p + q]*)
```

```mathematica
ex2 = FeynAmpDenominatorSplit[ex]
```

$$\frac{1}{q^2 (q-p)^2}$$

```mathematica
ex2 // FCE // StandardForm

(*FAD[q] FAD[-p + q]*)
```