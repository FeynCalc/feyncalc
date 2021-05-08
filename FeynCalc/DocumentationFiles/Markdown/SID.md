##  SID 

SID[mu]  can be used as input for D-1-dimensional $\sigma ^{\mu }$ with D-dimensional Lorentz index μ and is transformed into PauliSigma[LorentzIndex[mu,D],D-1] by FeynCalcInternal..

###  Examples 

```mathematica
SID[\[Mu]] 
 
SID[\[Mu], \[Nu]] - SID[\[Nu], \[Mu]] 
 
StandardForm[FCI[SID[\[Mu]]]] 
 
SID[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
StandardForm[SID[\[Mu], \[Nu], \[Rho], \[Sigma]]] 
 
SID[\[Alpha]] . (SISD[p] + m) . SID[\[Beta]]
```

$$\sigma ^{\mu }$$

$$\sigma ^{\mu }.\sigma ^{\nu }-\sigma ^{\nu }.\sigma ^{\mu }$$

```
(*PauliSigma[LorentzIndex[\[Mu], D], -1 + D]*)
```

$$\sigma ^{\mu }.\sigma ^{\nu }.\sigma ^{\rho }.\sigma ^{\sigma }$$

```
(*SID[\[Mu]] . SID[\[Nu]] . SID[\[Rho]] . SID[\[Sigma]]*)
```

$$\sigma ^{\alpha }.(m+\sigma \cdot p).\sigma ^{\beta }$$