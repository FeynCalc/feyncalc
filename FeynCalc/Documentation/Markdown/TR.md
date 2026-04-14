## TR

`TR[exp]` calculates the Dirac trace of `exp`.

If the option `SUNSimplify` is set to `True` (default), $SU(N)$ algebra is simplified as well.

Notice that `TR` is a legacy function that should not be used in new codes. Instead, you can wrap your string Dirac matrices with `DiracTrace` and subsequently apply `DiracSimplify` to calculate the trace.

### See also

[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrace](DiracTrace.md), [SUNSimplify](SUNSimplify.md).

### Examples

```mathematica
GA[\[Mu], \[Nu]] 
 
TR[%]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$4 \bar{g}^{\mu \nu }$$

```mathematica
TR[(GSD[p] + m) . GAD[\[Mu]] . (GSD[q] - m) . GAD[\[Nu]]]
```

$$-4 \left(m^2 g^{\mu \nu }+g^{\mu \nu } (p\cdot q)-p^{\nu } q^{\mu }-p^{\mu } q^{\nu }\right)$$

```mathematica
TR[GA[\[Mu], \[Nu], \[Rho], \[Sigma], 5]]
```

$$-4 i \bar{\epsilon }^{\mu \nu \rho \sigma }$$

```mathematica
TR[GS[p, q, r, s]]
```

$$4 \left(\left(\overline{p}\cdot \overline{s}\right) \left(\overline{q}\cdot \overline{r}\right)-\left(\overline{p}\cdot \overline{r}\right) \left(\overline{q}\cdot \overline{s}\right)+\left(\overline{p}\cdot \overline{q}\right) \left(\overline{r}\cdot \overline{s}\right)\right)$$

```mathematica
TR[(GS[p] + m) . GA[\[Mu]] . (GS[q] + m) . GA[\[Mu]], Factoring -> True]
```

$$8 \left(2 m^2-\overline{p}\cdot \overline{q}\right)$$

```mathematica
TR[GA[\[Alpha], \[Beta]], FCE -> True]
```

$$4 \bar{g}^{\alpha \beta }$$

```mathematica
GA[\[Mu], \[Nu]] SUNT[b] . SUNT[c] SUNDelta[c, b] 
 
TR[%]

```mathematica

$$\delta ^{bc} T^b.T^c \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$4 C_F \bar{g}^{\mu \nu }$$