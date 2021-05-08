##  GA 

GA[μ] can be used as input for a 4-dimensional $\gamma ^{\mu }{}_$and is transformed into DiracGamma[LorentzIndex[$\mu$]] by FeynCalcInternal (=FCI).GA[$\mu ,\nu ,$ ...] is a short form for GA[$\mu$].GA[$\nu$]..

###  See also 

DiracGamma, GAD, GS.

###  Examples 

```mathematica
GA[\[Mu]] 
 
GA[\[Mu], \[Nu]] - GA[\[Nu], \[Mu]] 
 
StandardForm[FCI[GA[\[Mu]]]] 
 
GA[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
StandardForm[GA[\[Mu], \[Nu], \[Rho], \[Sigma]]] 
 
GA[\[Alpha]] . (GS[p] + m) . GA[\[Beta]]
```

$$\bar{\gamma }^{\mu }$$

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }-\bar{\gamma }^{\nu }.\bar{\gamma }^{\mu }$$

```
(*DiracGamma[LorentzIndex[\[Mu]]]*)
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }$$

```
(*GA[\[Mu]] . GA[\[Nu]] . GA[\[Rho]] . GA[\[Sigma]]*)
```

$$\bar{\gamma }^{\alpha }.\left(\bar{\gamma }\cdot \overline{p}+m\right).\bar{\gamma }^{\beta }$$