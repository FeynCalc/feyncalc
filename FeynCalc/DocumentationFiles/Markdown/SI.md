##  SI 

SI[mu] can be used as input for 3-dimensional $\sigma ^{\mu }$ with 4-dimensional Lorentz index μ and is transformed into PauliSigma[LorentzIndex[mu]] by FeynCalcInternal..

###  Examples 

```mathematica
SI[\[Mu]] 
 
SI[\[Mu], \[Nu]] - SI[\[Nu], \[Mu]] 
 
StandardForm[FCI[SI[\[Mu]]]] 
 
SI[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
StandardForm[SI[\[Mu], \[Nu], \[Rho], \[Sigma]]] 
 
SI[\[Alpha]] . (SIS[p] + m) . SI[\[Beta]]
```

$$\bar{\sigma }^{\mu }$$

$$\bar{\sigma }^{\mu }.\bar{\sigma }^{\nu }-\bar{\sigma }^{\nu }.\bar{\sigma }^{\mu }$$

```
(*PauliSigma[LorentzIndex[\[Mu]]]*)
```

$$\bar{\sigma }^{\mu }.\bar{\sigma }^{\nu }.\bar{\sigma }^{\rho }.\bar{\sigma }^{\sigma }$$

```
(*SI[\[Mu]] . SI[\[Nu]] . SI[\[Rho]] . SI[\[Sigma]]*)
```

$$\bar{\sigma }^{\alpha }.\left(\bar{\sigma }\cdot \overline{p}+m\right).\bar{\sigma }^{\beta }$$