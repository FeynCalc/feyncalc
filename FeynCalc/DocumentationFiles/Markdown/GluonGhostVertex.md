##  GluonGhostVertex 

`GluonGhostVertex[{p, mu, a}, {q, nu, b}, {k, rho, c}]` or `GluonGhostVertex[ p, mu, a , q, nu, b , k, rho, c]` yields the Gluon-Ghost vertex. The first argument represents the gluon and the third argument the outgoing ghost field (but incoming 4-momentum).

`GGV` can be used as an abbreviation of `GluonGhostVertex`.The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`.

###  See also 

GluonPropagator, GluonSelfEnergy, GluonVertex, QCDFeynmanRuleConvention, GhostPropagator.

###  Examples 

```mathematica
GluonGhostVertex[{p, \[Mu], a}, {q, \[Nu], b}, {k, \[Rho], c}] 
Explicit[%]
```

$$\tilde{\Lambda }^{\mu }(k) f^{abc}$$

$$g_s \left(-k^{\mu }\right) f^{abc}$$