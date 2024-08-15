## TR

`TR[exp]` calculates the Dirac trace of `exp`. Depending on the setting of the option `SUNTrace` also a trace over $SU(N)$ objects is performed.

`TR[list]` finds the trace of the matrix or tensor list.

`TR[list, f]` finds a generalized trace, combining terms with f instead of `Plus`.

`TR[list, f, n]` goes down to level `n` in `list`.

`TR[expression]` calculates the `DiracTrace`, i.e., `TR[expression]` if any of `DiracGamma`, `GA`, `GAD`, `GS` or `GSD` is present in expression.

### See also

[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrace](DiracTrace.md), [FermionSpinSum](FermionSpinSum.md), [SUNTrace](SUNTrace.md).

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
 
TR[%, SUNTrace -> False, SUNNToCACF -> True] 
 
TR[%%, SUNTrace -> True, SUNNToCACF -> True]
```

$$\delta ^{bc} T^b.T^c \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

![0ot3t654zfwoj](img/0ot3t654zfwoj.svg)

![0ghd8b8tpozaa](img/0ghd8b8tpozaa.svg)

![0hz7g0pbjf1j9](img/0hz7g0pbjf1j9.svg)

![0hw442jsqmu7m](img/0hw442jsqmu7m.svg)

$$4 C_F \bar{g}^{\mu \nu }$$

![0yp1tcn7js8vw](img/0yp1tcn7js8vw.svg)

![1ws0yyvp2z6rk](img/1ws0yyvp2z6rk.svg)

![14q9vfcl2ne33](img/14q9vfcl2ne33.svg)

![078irs7exvkqw](img/078irs7exvkqw.svg)

$$4 C_F \bar{g}^{\mu \nu }$$

```mathematica
TR[1, SUNTrace -> False, SUNNToCACF -> True]
```

![092oi250umo62](img/092oi250umo62.svg)

![1kxrxifnbycah](img/1kxrxifnbycah.svg)

![0ejd5087k1e3u](img/0ejd5087k1e3u.svg)

![0r8qqzg2cmsx4](img/0r8qqzg2cmsx4.svg)

$$4$$

```mathematica
TR[1, SUNTrace -> True, SUNNToCACF -> True]
```

![0hdji48sjs0uw](img/0hdji48sjs0uw.svg)

![1va2upktn7wft](img/1va2upktn7wft.svg)

![1fzren43hocdr](img/1fzren43hocdr.svg)

![12gtp4iazvrd5](img/12gtp4iazvrd5.svg)

$$4$$