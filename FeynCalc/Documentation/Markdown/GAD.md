## GAD

`GAD[mu]` can be used as input for a $D$-dimensional $\gamma ^{\mu }$ and is transformed into `DiracGamma[LorentzIndex[mu,D],D]` by `FeynCalcInternal` (=`FCI`).

`GAD[mu , nu , ...]` is a short form for `GAD[mu].GAD[nu]`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [GS](GS.md).

### Examples

```mathematica
GAD[\[Mu]]
```

$$\gamma ^{\mu }$$

```mathematica
GAD[\[Mu], \[Nu]] - GAD[\[Nu], \[Mu]]
```

$$\gamma ^{\mu }.\gamma ^{\nu }-\gamma ^{\nu }.\gamma ^{\mu }$$

```mathematica
StandardForm[FCI[GAD[\[Mu]]]]

(*DiracGamma[LorentzIndex[\[Mu], D], D]*)
```

```mathematica
GAD[\[Mu], \[Nu], \[Rho], \[Sigma]]
```

$$\gamma ^{\mu }.\gamma ^{\nu }.\gamma ^{\rho }.\gamma ^{\sigma }$$

```mathematica
StandardForm[GAD[\[Mu], \[Nu], \[Rho], \[Sigma]]]

(*GAD[\[Mu]] . GAD[\[Nu]] . GAD[\[Rho]] . GAD[\[Sigma]]*)
```

```mathematica
GAD[\[Alpha]] . (GSD[p] + m) . GAD[\[Beta]]
```

$$\gamma ^{\alpha }.(m+\gamma \cdot p).\gamma ^{\beta }$$