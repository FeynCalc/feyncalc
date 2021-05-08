##  GAE 

`GAE[μ]` can be used as input for a `D-4`-dimensional $\gamma^\mu$and is transformed into `DiracGamma[LorentzIndex[$\mu$,D-4],D-4]` by FeynCalcInternal (=FCI). `GAE[$\mu$, $\nu$ , ...]` is a short form for `GAE[$\mu$].GAE[$\nu$]. ...`.

###  See also 

DiracGamma, GA, GS, GAD.

###  Examples 

```mathematica
GAE[\[Mu]]
```

$$\hat{\gamma }^{\mu }$$

```mathematica
GAE[\[Mu], \[Nu]] - GAE[\[Nu], \[Mu]]
```

$$\hat{\gamma }^{\mu }.\hat{\gamma }^{\nu }-\hat{\gamma }^{\nu }.\hat{\gamma }^{\mu }$$

```mathematica
StandardForm[FCI[GAE[\[Mu]]]]

(*DiracGamma[LorentzIndex[\[Mu], -4 + D], -4 + D]*)
```

```mathematica
GAE[\[Mu], \[Nu], \[Rho], \[Sigma]]
```

$$\hat{\gamma }^{\mu }.\hat{\gamma }^{\nu }.\hat{\gamma }^{\rho }.\hat{\gamma }^{\sigma }$$

```mathematica
StandardForm[GAE[\[Mu], \[Nu], \[Rho], \[Sigma]]]

(*GAE[\[Mu]] . GAE[\[Nu]] . GAE[\[Rho]] . GAE[\[Sigma]]*)
```

```mathematica
GAE[\[Alpha]] FVD[p, \[Alpha]] // Contract
```

$$\hat{\gamma }\cdot \hat{p}$$

```mathematica
GAE[\[Alpha]] FV[p, \[Alpha]] // Contract
```

$$0$$

In order to use Dirac algebra with `D-4` dimensional objects you need to activate the t'Hooft-Veltman-Breitenlohner-Maison scheme first

```mathematica
FCSetDiracGammaScheme["NDR"]
DiracSimplify[GAE[\[Mu]] . GAD[\[Mu]]]
```

$$\text{NDR}$$

$$\text{}\text{DiracTrace}\text{::}\text{mixmsg}: \text{Expressions that mix D-, 4- and D-4-dimensional quantities are forbidden in Dirac matrix chains unless you are using the t'Hooft-Veltman scheme. For every other scheme, please recheck your input expressions and ensure that all matrices, spinors and tensors are purely D-dimensional. You might want to use FCGetDimensions[exp] to find the offending terms and fix them by hand or ChangeDimension[exp,D] to convert the whole expression to D-dimensions. If you explicitly intend to use the t'Hooft-Veltman scheme, please activate it via FCSetDiracGammaScheme[{``}BMHV{''}].}$$

$$\text{$\$$Aborted}$$

```mathematica
FCSetDiracGammaScheme["BMHV"]
DiracSimplify[GAE[\[Mu]] . GAD[\[Mu]]]
```

$$\text{BMHV}$$

$$D-4$$

```mathematica
FCSetDiracGammaScheme["NDR"]
```

$$\text{NDR}$$