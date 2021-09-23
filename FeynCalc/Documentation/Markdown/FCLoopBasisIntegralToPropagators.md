## FCLoopBasisIntegralToPropagators

`FCLoopBasisIntegralToPropagators[int, {q1, q2, ...}]` is an auxiliary function that converts the loop integral int that depends on the loop momenta `q1, q2, ...` to a list of propagators and scalar products. All propagators and scalar products that do not depend on the loop momenta are discarded, unless the `Rest` option is set to `True`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
SFAD[p1]
FCLoopBasisIntegralToPropagators[%, {p1}]
```

$$\frac{1}{(\text{p1}^2+i \eta )}$$

$$\left\{\frac{1}{(\text{p1}^2+i \eta )}\right\}$$

```mathematica
SFAD[p1, p2]
FCLoopBasisIntegralToPropagators[%, {p1, p2}]
```

$$\frac{1}{(\text{p1}^2+i \eta ).(\text{p2}^2+i \eta )}$$

$$\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )}\right\}$$

```mathematica
SPD[q, p] SFAD[q, q - p, q - p]
FCLoopBasisIntegralToPropagators[%, {q}]
```

$$\frac{p\cdot q}{(q^2+i \eta ).((q-p)^2+i \eta )^2}$$

$$\left\{(p\cdot q+i \eta ),\frac{1}{(q^2+i \eta )},\frac{1}{((q-p)^2+i \eta )}\right\}$$