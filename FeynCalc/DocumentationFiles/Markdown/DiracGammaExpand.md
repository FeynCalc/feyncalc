## DiracGammaExpand

`DiracGammaExpand[exp]` expands Dirac matrices contracted to linear combinations of $4$-vectors. All `DiracGamma[Momentum[a+b+ ...]]` will be expanded to `DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + DiracGamma[Momentum[...]]` .

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [DiracGammaCombine](DiracGammaCombine.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).

### Examples

```mathematica
GS[q] . GS[p - q]
DiracGammaExpand[%]
StandardForm[%]
```

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \left(\overline{p}-\overline{q}\right)\right)$$

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}-\bar{\gamma }\cdot \overline{q}\right)$$

```
(*DiracGamma[Momentum[q]] . (DiracGamma[Momentum[p]] - DiracGamma[Momentum[q]])*)
```

`DiracGammaExpand` rewrites $\gamma^{\mu } (p-q)_{\mu }$ as $\gamma^{mu } p_{mu } - \gamma^{\mu } q_{\mu }$.

The inverse operation is `DiracGammaCombine`.

```mathematica
GS[q] . (GS[p] - GS[q])
DiracGammaCombine[%]
StandardForm[%]
```

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}-\bar{\gamma }\cdot \overline{q}\right)$$

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \left(\overline{p}-\overline{q}\right)\right)$$

```
(*DiracGamma[Momentum[q]] . DiracGamma[Momentum[p - q]]*)
```

It is possible to perform the expansions only on Dirac matrices contracted with particular momenta.

```mathematica
c1 GAD[\[Mu]] . (GSD[p1 + p2] + m) . GAD[\[Nu]] + c2 GAD[\[Mu]] . (GSD[q1 + q2] + m) . GAD[\[Nu]]
DiracGammaExpand[%, Momentum -> {q1}]
```

$$\text{c1} \gamma ^{\mu }.(m+\gamma \cdot (\text{p1}+\text{p2})).\gamma ^{\nu }+\text{c2} \gamma ^{\mu }.(m+\gamma \cdot (\text{q1}+\text{q2})).\gamma ^{\nu }$$

$$\text{c1} \gamma ^{\mu }.(m+\gamma \cdot (\text{p1}+\text{p2})).\gamma ^{\nu }+\text{c2} \gamma ^{\mu }.(m+\gamma \cdot \;\text{q1}+\gamma \cdot \;\text{q2}).\gamma ^{\nu }$$

If the input expression contains `DiracSigma`,  `DiracGammaExpand` will expand Feynman slashes inside `DiracSigma` and call `DiracSigmaExpand`.

```mathematica
DiracSigma[GSD[p + q], GSD[r]]
DiracGammaExpand[%]
```

$$\sigma ^{p+qr}$$

$$\sigma ^{pr}+\sigma ^{qr}$$

The call to `DiracSigmaExpand` can be inhibited by disabling the corresponding option.

```mathematica
DiracGammaExpand[DiracSigma[GSD[p + q], GSD[r]], DiracSigmaExpand -> False]
```

$$\text{DiracSigma}(\gamma \cdot p+\gamma \cdot q,\gamma \cdot r)$$

Use `DiracSimplify` for noncommutative expansions with the corresponding simplifications.

```mathematica
DiracSimplify[GS[q] . (GS[p - q])]
```

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}\right)-\overline{q}^2$$

If simplifications are not required, you may also combine `DiracGammaExpand` with `DotSimplify`.

```mathematica
DotSimplify[DiracGammaExpand[GS[q] . (GS[p - q])]]
```

$$\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{p}\right)-\left(\bar{\gamma }\cdot \overline{q}\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$
