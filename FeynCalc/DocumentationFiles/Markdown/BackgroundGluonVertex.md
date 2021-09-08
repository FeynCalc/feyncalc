## BackgroundGluonVertex

`BackgroundGluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}]` yields the 3-gluon vertex in the background field gauge, where the first set of arguments corresponds to the external background field.   `BackgroundGluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}, {s, si, d}]` yields the 4-gluon vertex, with `{p, mu ,a}` and `{k, la, c}` denoting the external background fields.

The gauge, dimension and the name of the coupling constant are determined by the options `Gauge`, `Dimension` and `CouplingConstant`.

The Feynman rules are taken from L. Abbot NPB 185 (1981), 189-203; except that all momenta are incoming. Note that Abbot's coupling constant convention is consistent with the default setting of `GluonVertex`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
BackgroundGluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {k, \[Lambda], c}]
```

$$g_s f^{abc} \left(g^{\mu \nu } (-k+p-q)^{\lambda }+g^{\lambda \mu } (k-p+q)^{\nu }+g^{\lambda \nu } (q-k)^{\mu }\right)$$

```mathematica
BackgroundGluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {k, \[Lambda], c}, {s, \[Sigma], d}]
```

$$-i g_s^2 \left(f^{ad\text{FCGV}(\text{u24})} f^{bc\text{FCGV}(\text{u24})} \left(g^{\lambda \sigma } g^{\mu \nu }-g^{\lambda \nu } g^{\mu \sigma }-g^{\lambda \mu } g^{\nu \sigma }\right)+f^{ac\text{FCGV}(\text{u24})} f^{bd\text{FCGV}(\text{u24})} \left(g^{\lambda \sigma } g^{\mu \nu }-g^{\lambda \nu } g^{\mu \sigma }\right)+f^{ab\text{FCGV}(\text{u24})} f^{cd\text{FCGV}(\text{u24})} \left(g^{\lambda \sigma } g^{\mu \nu }-g^{\lambda \nu } g^{\mu \sigma }+g^{\lambda \mu } g^{\nu \sigma }\right)\right)$$

```mathematica
BackgroundGluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {k, \[Lambda], c},Gauge -> \[Alpha]]
```

$$g_s f^{abc} \left(g^{\mu \nu } \left(-\frac{k}{\alpha }+p-q\right)^{\lambda }+g^{\lambda \mu } \left(k-p+\frac{q}{\alpha }\right)^{\nu }+g^{\lambda \nu } (q-k)^{\mu }\right)$$

```mathematica
BackgroundGluonVertex[{p, \[Mu], a}, {q, \[Nu], b}, {k, \[Lambda], c}, {s, \[Sigma], d}, Gauge -> \[Alpha]]
```

$$-i g_s^2 \left(f^{ad\text{FCGV}(\text{u25})} f^{bc\text{FCGV}(\text{u25})} \left(-\frac{g^{\lambda \nu } g^{\mu \sigma }}{\alpha }+g^{\lambda \sigma } g^{\mu \nu }-g^{\lambda \mu } g^{\nu \sigma }\right)+f^{ab\text{FCGV}(\text{u25})} f^{cd\text{FCGV}(\text{u25})} \left(\frac{g^{\lambda \sigma } g^{\mu \nu }}{\alpha }-g^{\lambda \nu } g^{\mu \sigma }+g^{\lambda \mu } g^{\nu \sigma }\right)+f^{ac\text{FCGV}(\text{u25})} f^{bd\text{FCGV}(\text{u25})} \left(g^{\lambda \sigma } g^{\mu \nu }-g^{\lambda \nu } g^{\mu \sigma }\right)\right)$$
