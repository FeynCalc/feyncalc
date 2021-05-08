##  EpsChisholm 

`EpsChisholm[exp]` applies the Chisholm identiy to a Dirac matrix contracted with a Levi-Civita tensor.

###  See also 

Chisholm, Eps, DiracGamma

###  Examples 

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]] GA[\[Sigma], 5]
EpsChisholm[%]
```

$$\bar{\gamma }^{\sigma }.\bar{\gamma }^5 \bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$-i \bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }+i \bar{\gamma }^{\rho } \bar{g}^{\mu \nu }-i \bar{\gamma }^{\nu } \bar{g}^{\mu \rho }+i \bar{\gamma }^{\mu } \bar{g}^{\nu \rho }$$