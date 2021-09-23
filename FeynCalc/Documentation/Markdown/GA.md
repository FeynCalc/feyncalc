## GA

`GA[mu]` can be used as input for a 4-dimensional $\gamma^{\mu }$ and is transformed into `DiracGamma[LorentzIndex[mu]]` by FeynCalcInternal (=FCI).

`GA[mu , nu , ...]` is a short form for `GA[mu].GA[nu]`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GAD](GAD.md), [GS](GS.md).

### Examples

```mathematica
GA[\[Mu]]
```

$$\bar{\gamma }^{\mu }$$

```mathematica
GA[\[Mu], \[Nu]] - GA[\[Nu], \[Mu]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }-\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu }$$

```mathematica
StandardForm[FCI[GA[\[Mu]]]]

(*DiracGamma[LorentzIndex[\[Mu]]]*)
```

```mathematica
GA[\[Mu], \[Nu], \[Rho], \[Sigma]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }$$

```mathematica
StandardForm[GA[\[Mu], \[Nu], \[Rho], \[Sigma]]]

(*GA[\[Mu]] . GA[\[Nu]] . GA[\[Rho]] . GA[\[Sigma]]*)
```

```mathematica
GA[\[Alpha]] . (GS[p] + m) . GA[\[Beta]]
```

$$\bar{\gamma }^{\alpha }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\beta }$$
