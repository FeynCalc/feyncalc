##  FCLoopBasisCreateScalarProducts 

FCLoopBasisCreateScalarProducts {q1, q2, ...}, {p1, p2, ...}, {d1, d2, ...}, head] generates a list of all loop-momentum dependent scalar products made out of the loop momenta q1, q2, ... and external momenta p1, p2, ... in the space-time dimensions d1, d2, .... The argument head can be Pair to generate Lorentzian scalar products or CartesianPair to generate Cartesian scalar products..

###  Examples 

```mathematica
FCLoopBasisCreateScalarProducts[{l}, {}, {D}, Pair]
```

$$\text{FCLoopBasisCreateScalarProducts}(\{l\},\{\},\{D\},\text{Pair})$$

```mathematica
FCLoopBasisCreateScalarProducts[{l}, {p1, p2}, {4}, Pair]
```

$$\text{FCLoopBasisCreateScalarProducts}(\{l\},\{\text{p1},\text{p2}\},\{4\},\text{Pair})$$

```mathematica
FCLoopBasisCreateScalarProducts[{l}, {}, {D - 1}, CartesianPair]
```

$$\text{FCLoopBasisCreateScalarProducts}(\{l\},\{\},\{D-1\},\text{CartesianPair})$$