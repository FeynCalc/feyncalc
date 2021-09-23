## Trick

`Trick[exp]` performs several basic simplifications without expansion. `Trick[exp]` uses `Contract`, `DotSimplify` and `SUNDeltaContract`.

### See also

[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [Contract](Contract.md), [DiracTrick](DiracTrick.md), [DotSimplify](DotSimplify.md), [DiracTrick](DiracTrick.md).

### Examples

This calculates $g^{\mu  \nu} \gamma _{\mu }$ and $g_{\nu }^{\nu}$ in $D$ dimensions.

```mathematica
Trick[{GA[\[Mu]] MT[\[Mu], \[Nu]], MTD[\[Nu], \[Nu]]}]
```

$$\left\{\bar{\gamma }^{\nu },D\right\}$$

```mathematica
FV[p + r, \[Mu]] MT[\[Mu], \[Nu]] FV[q - p, \[Nu]]
Trick[%]
```

$$\bar{g}^{\mu \nu } \left(\overline{q}-\overline{p}\right)^{\nu } \left(\overline{p}+\overline{r}\right)^{\mu }$$

$$\overline{p}\cdot \overline{q}-\overline{p}\cdot \overline{r}-\overline{p}^2+\overline{q}\cdot \overline{r}$$

```mathematica
Trick[c . b . a . GA[d] . GA[e]]
% // FCE // StandardForm
```

$$a b c \bar{\gamma }^d.\bar{\gamma }^e$$

```
(*a b c GA[d] . GA[e]*)
```