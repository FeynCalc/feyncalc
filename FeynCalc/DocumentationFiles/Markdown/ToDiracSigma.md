##  ToDiracSigma 

`ToDiracSigma[exp, x, y]` substitutes the neighboring Dirac matrices $x$ and $y$ by `DiracSigma` and the metric tensor.

###  See also 

DiracGamma, DiracSigma, DiracSigmaExplicit.

###  Examples 

```mathematica
GA[\[Mu], \[Nu]]
ToDiracSigma[%, GA[\[Mu]], GA[\[Nu]]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$\bar{g}^{\mu \nu }-i \sigma ^{\mu \nu }$$

```mathematica
GA[\[Mu], \[Nu], \[Alpha], \[Beta], \[Rho], \[Sigma]]
ToDiracSigma[%, GA[\[Alpha]], GA[\[Beta]]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }$$

$$\bar{g}^{\alpha \beta } \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }-i \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\sigma ^{\alpha \beta }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\sigma }$$