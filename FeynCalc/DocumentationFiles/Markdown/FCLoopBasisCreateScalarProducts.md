## FCLoopBasisCreateScalarProducts

`FCLoopBasisCreateScalarProducts {q1, q2, ...}, {p1, p2, ...}, {d1, d2, ...}, head]` generates a list of all loop-momentum dependent scalar products made out of the loop momenta `q1, q2, ...` and external momenta `p1, p2, ...` in the space-time dimensions `d1, d2, ...`. The argument `head` can be `Pair` to generate Lorentzian scalar products or `CartesianPair` to generate Cartesian scalar products.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCLoopBasisCreateScalarProducts[{l}, {}, {D}, Pair]
```

$$\left\{l^2\right\}$$

```mathematica
FCLoopBasisCreateScalarProducts[{l}, {p1, p2}, {4}, Pair]
```

$$\left\{\overline{l}^2,\overline{l}\cdot \overline{\text{p1}},\overline{l}\cdot \overline{\text{p2}}\right\}$$

```mathematica
FCLoopBasisCreateScalarProducts[{l}, {}, {D - 1}, CartesianPair]
```

$$\left\{l^2\right\}$$
