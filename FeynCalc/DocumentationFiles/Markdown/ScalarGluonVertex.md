## ScalarGluonVertex

`ScalarGluonVertex[{p}, {q}, {μ, a}]` or `ScalarGluonVertex[p,  q,  μ, a]` yields the scalar-scalar-gluon vertex, where `p` and `q` are incoming momenta.

`ScalarGluonVertex[{μ, a}, {ν, b}]` yields the scalar-scalar-gluon-gluon vertex, where `p` and `q` are incoming momenta.

The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
ScalarGluonVertex[{p}, {q}, {\[Mu], a}]
```

$$i T^a g_s (p-q)^{\mu }$$
