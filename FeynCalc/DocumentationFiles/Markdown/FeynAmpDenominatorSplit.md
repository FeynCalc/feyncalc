## FeynAmpDenominatorSplit

`FeynAmpDenominatorSplit[expr]` splits all `FeynAmpDenominator[a,b, ...]` in `expr` into `FeynAmpDenominator[a]*FeynAmpDenominator[b]*...` . `FeynAmpDenominatorSplit[expr,  Momentum ->q1]` splits all `FeynAmpDenominator` in expr into two products, one containing `q1` and other momenta, the second being free of `q1`.

### See also

[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorCombine](FeynAmpDenominatorCombine.md).

### Examples

```mathematica
FAD[q1, q1 - p, q1 - q2, q2, q2 - p]
FeynAmpDenominatorSplit[%]
% // FCE // StandardForm
```

$$\frac{1}{\text{q1}^2.(\text{q1}-p)^2.(\text{q1}-\text{q2})^2.\text{q2}^2.(\text{q2}-p)^2}$$

$$\frac{1}{\text{q1}^2 \;\text{q2}^2 (\text{q1}-p)^2 (\text{q2}-p)^2 (\text{q1}-\text{q2})^2}$$

```
(*FAD[q1] FAD[-p + q1] FAD[q1 - q2] FAD[q2] FAD[-p + q2]*)
```

```mathematica
FeynAmpDenominatorSplit[FAD[q1, q1 - p, q1 - q2, q2, q2 - p], Momentum -> {q1}]
% // FCE // StandardForm
FeynAmpDenominatorCombine[%] // FCE // StandardForm
```

$$\frac{1}{\text{q2}^2.(\text{q2}-p)^2 \;\text{q1}^2.(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}$$

```
(*FAD[q2, -p + q2] FAD[q1, -p + q1, q1 - q2]*)

(*FAD[q1, q2, q1 - q2, -p + q1, -p + q2]*)
```
