## ScalarGluonVertex

`ScalarGluonVertex[{p}, {q}, {mu, a}]` or `ScalarGluonVertex[p,  q,  mu, a]` yields the scalar-scalar-gluon vertex, where `p` and `q` are incoming momenta.

`ScalarGluonVertex[{mu, a}, {nu, b}]` yields the scalar-scalar-gluon-gluon vertex, where `p` and `q` are incoming momenta.

The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
ScalarGluonVertex[{p}, {q}, {\[Mu], a}]
```

$$i T^a g_s (p-q)^{\mu }$$