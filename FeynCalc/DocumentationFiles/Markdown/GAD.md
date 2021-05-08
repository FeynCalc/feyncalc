##  GAD 

GAD[μ] can be used as input for a D-dimensional $\gamma ^{\mu }{}_$and is transformed into DiracGamma[LorentzIndex[$\mu$,D],D] by FeynCalcInternal (=FCI).GAD[$\mu ,\nu , \text{...}$] is a short form for GAD[$\mu$].GAD[$\nu$]. ... ..

###  See also 

DiracGamma, GA, GS.

###  Examples 

```mathematica
GAD[\[Mu]] 
 
GAD[\[Mu], \[Nu]] - GAD[\[Nu], \[Mu]] 
 
StandardForm[FCI[GAD[\[Mu]]]] 
 
GAD[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
StandardForm[GAD[\[Mu], \[Nu], \[Rho], \[Sigma]]] 
 
GAD[\[Alpha]] . (GSD[p] + m) . GAD[\[Beta]]
```

$$\gamma ^{\mu }$$

$$\gamma ^{\mu }.\gamma ^{\nu }-\gamma ^{\nu }.\gamma ^{\mu }$$

```
(*DiracGamma[LorentzIndex[\[Mu], D], D]*)
```

$$\gamma ^{\mu }.\gamma ^{\nu }.\gamma ^{\rho }.\gamma ^{\sigma }$$

```
(*GAD[\[Mu]] . GAD[\[Nu]] . GAD[\[Rho]] . GAD[\[Sigma]]*)
```

$$\gamma ^{\alpha }.(m+\gamma \cdot p).\gamma ^{\beta }$$