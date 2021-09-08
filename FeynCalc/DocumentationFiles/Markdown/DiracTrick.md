## DiracTrick

`DiracTrick[exp]` contracts Dirac matrices with each other and performs several simplifications but no expansions.There are not many cases when a user will need to call this function directly. Use `DiracSimplify` to achieve maximal simplification of Dirac matrix chains. Regarding the treatment of $\gamma^5$ in $D$-dimensional expressions or the evaluation of expressions with tensors living in different dimensions, see the explanations on the help pages for `DiracSimplify` and `DiracTrace`.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [DiracEquation](DiracEquation.md), [DiracGamma](DiracGamma.md), [DiracGammaExpand](DiracGammaExpand.md), [DiracTrick](DiracTrick.md), [SirlinSimplify](SirlinSimplify.md), [SpinorChainTrick](SpinorChainTrick.md).

### Examples

When applied to chains of Dirac matrices that do not require noncommutative expansions, contractions with other tensors, simplifications of spinor chains or evaluations of Dirac traces,  `DiracTrick` will produce results similar to those of `DiracSimplify`.

```mathematica
GA[\[Mu], \[Nu], \[Mu]]
DiracTrick[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu }$$

$$-2 \bar{\gamma }^{\nu }$$

```mathematica
GS[p] . GS[p]
DiracTrick[%]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$\overline{p}^2$$

```mathematica
GA[5, \[Mu], \[Nu]]
DiracTrick[%]
```

$$\bar{\gamma }^5.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^5$$

```mathematica
(1/2 - GA[5]/2) . (-((a + GS[p + q])/b)) . (1/2 + GA[5]/2)
DiracTrick[%]
```

$$\left(\frac{1}{2}-\frac{\bar{\gamma }^5}{2}\right).\left(-\frac{\bar{\gamma }\cdot \left(\overline{p}+\overline{q}\right)+a}{b}\right).\left(\frac{\bar{\gamma }^5}{2}+\frac{1}{2}\right)$$

$$-\frac{\left(\bar{\gamma }\cdot \left(\overline{p}+\overline{q}\right)\right).\bar{\gamma }^6}{b}$$

Dirac traces are not evaluated by `DiracTrick`

```mathematica
DiracTrace[GAD[\[Mu], \[Nu]]]
DiracTrick[%]
```

$$\text{tr}\left(\gamma ^{\mu }.\gamma ^{\nu }\right)$$

$$\text{tr}\left(\gamma ^{\mu }.\gamma ^{\nu }\right)$$
