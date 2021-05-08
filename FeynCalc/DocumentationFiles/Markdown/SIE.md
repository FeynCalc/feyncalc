##  SIE 

SIE[mu] can be used as input for D-1-dimensional $\sigma ^{\mu }$ with D-4-dimensional Lorentz index μ and is transformed into PauliSigma[LorentzIndex[mu,D-4],D-4] by FeynCalcInternal..

###  Examples 

```mathematica
SIE[\[Mu]] 
 
SIE[\[Mu], \[Nu]] - SIE[\[Nu], \[Mu]] 
 
StandardForm[FCI[SIE[\[Mu]]]] 
 
SIE[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
StandardForm[SIE[\[Mu], \[Nu], \[Rho], \[Sigma]]] 
 
SIE[\[Alpha]] . (SISE[p] + m) . SIE[\[Beta]]
```

$$\hat{\sigma }^{\mu }$$

$$\hat{\sigma }^{\mu }.\hat{\sigma }^{\nu }-\hat{\sigma }^{\nu }.\hat{\sigma }^{\mu }$$

```
(*PauliSigma[LorentzIndex[\[Mu], -4 + D], -4 + D]*)
```

$$\hat{\sigma }^{\mu }.\hat{\sigma }^{\nu }.\hat{\sigma }^{\rho }.\hat{\sigma }^{\sigma }$$

```
(*SIE[\[Mu]] . SIE[\[Nu]] . SIE[\[Rho]] . SIE[\[Sigma]]*)
```

$$\hat{\sigma }^{\alpha }.\left(m+\hat{\sigma }\cdot \hat{p}\right).\hat{\sigma }^{\beta }$$