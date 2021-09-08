## GluonVertex

`GluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}]` or `GluonVertex[p, mu, a, q, nu, b, k, la, c]` yields the 3-gluon vertex.    

`GluonVertex[{p, mu}, {q, nu}, {k, la}]` yields the 3-gluon vertex without color structure and the coupling constant.

`GluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}, {s, si, d}]` or `GluonVertex[{mu, a}, {nu, b}, {la, c}, {si, d}]` or `GluonVertex[p, mu, a, q, nu, b, k, la, c , s, si, d]` or `GluonVertex[mu, a, nu, b, la, c, si, d]` yields the 4-gluon vertex.

`GV` can be used as an abbreviation of `GluonVertex`.

The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`. All momenta are flowing into the vertex.

### See also

[Overview](Extra/FeynCalc.md), [GluonPropagator](GluonPropagator.md), [GluonGhostVertex](GluonGhostVertex.md).

### Examples

```mathematica
GluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {r, \[Rho], c}]
Explicit[%]
```

$$f^{abc} V^{\mu \nu \rho }(p\text{, }q\text{, }r)$$

$$g_s f^{abc} \left(g^{\mu \nu } \left(p^{\rho }-q^{\rho }\right)+g^{\mu \rho } \left(r^{\nu }-p^{\nu }\right)+g^{\nu \rho } \left(q^{\mu }-r^{\mu }\right)\right)$$

```mathematica
GV[{p, \[Mu]}, {q, \[Nu]}, {r, \[Rho]}]
Explicit[%]
```

$$V^{\mu \nu \rho }(p\text{, }q\text{, }r)$$

$$g_s \left(g^{\mu \nu } \left(p^{\rho }-q^{\rho }\right)+g^{\mu \rho } \left(r^{\nu }-p^{\nu }\right)+g^{\nu \rho } \left(q^{\mu }-r^{\mu }\right)\right)$$

```mathematica
GluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {r, \[Rho], c}, {s, \[Sigma], d}]
Explicit[%]
```

$$V_{abcd}^{\mu \nu \rho \sigma }(p\text{, }q\text{, }r\text{, }s)$$

$$-i g_s^2 \left(f^{ad\text{FCGV}(\text{u24})} f^{bc\text{FCGV}(\text{u24})} \left(g^{\mu \nu } g^{\rho \sigma }-g^{\mu \rho } g^{\nu \sigma }\right)+f^{ac\text{FCGV}(\text{u24})} f^{bd\text{FCGV}(\text{u24})} \left(g^{\mu \nu } g^{\rho \sigma }-g^{\mu \sigma } g^{\nu \rho }\right)+f^{ab\text{FCGV}(\text{u24})} f^{cd\text{FCGV}(\text{u24})} \left(g^{\mu \rho } g^{\nu \sigma }-g^{\mu \sigma } g^{\nu \rho }\right)\right)$$

```mathematica
GV[{\[Mu], a}, {\[Nu], b}, {\[Rho], c}, {\[Sigma], d}]
Explicit[%]
```

$$V^{abcd}$$

$$-i g_s^2 \left(f^{ad\text{FCGV}(\text{u25})} f^{bc\text{FCGV}(\text{u25})} \left(g^{\mu \nu } g^{\rho \sigma }-g^{\mu \rho } g^{\nu \sigma }\right)+f^{ac\text{FCGV}(\text{u25})} f^{bd\text{FCGV}(\text{u25})} \left(g^{\mu \nu } g^{\rho \sigma }-g^{\mu \sigma } g^{\nu \rho }\right)+f^{ab\text{FCGV}(\text{u25})} f^{cd\text{FCGV}(\text{u25})} \left(g^{\mu \rho } g^{\nu \sigma }-g^{\mu \sigma } g^{\nu \rho }\right)\right)$$
