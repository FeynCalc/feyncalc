## DiracGammaCombine

`DiracGammaCombine[exp]` is (nearly) the inverse operation to `DiracGammaExpand`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [DiracGammaExpand](DiracGammaExpand.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).

### Examples

```mathematica
GS[p] + GS[q] 
 
ex = DiracGammaCombine[%]
```

$$\bar{\gamma }\cdot \overline{p}+\bar{\gamma }\cdot \overline{q}$$

$$\bar{\gamma }\cdot \left(\overline{p}+\overline{q}\right)$$

```mathematica
ex // StandardForm

(*DiracGamma[Momentum[p + q]]*)
```

```mathematica
2 GSD[p] - 3 GSD[q] 
 
ex = DiracGammaCombine[%]
```

$$2 \gamma \cdot p-3 \gamma \cdot q$$

$$\gamma \cdot (2 p-3 q)$$

```mathematica
ex // StandardForm

(*DiracGamma[Momentum[2 p - 3 q, D], D]*)
```

```mathematica
DiracGammaCombine[2 GSD[p] - 3 GSD[q]] 
 
DiracGammaExpand[%]
```

$$\gamma \cdot (2 p-3 q)$$

$$2 \gamma \cdot p-3 \gamma \cdot q$$