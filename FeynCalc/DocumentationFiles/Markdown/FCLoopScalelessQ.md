`FCLoopScalelessQ[int, {p1, p2, ...}]` checks whether the loop integral `int` depending on the loop momenta `p1, p2, ...` is scaleless. Only integrals that admit a Feynman parametrization with proper $U$ and $F$ polynomials are supported.

The function uses the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868). Cf. also the PhD thesis of Jens Hoff [10.5445/IR/1000047447](https://doi.org/10.5445/IR/1000047447) for the detailed description of a possible implementation.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakOrder](FCLoopPakOrder.md), [FCLoopScalelessQ](FCLoopScalelessQ.md).

### Examples

A scaleless 2-loop tadpole

```mathematica
FCLoopScalelessQ[FAD[p1, p2, p1 - p2], {p1, p2}]
```

$$\text{True}$$

A 1-loop massless eikonal integral.

```mathematica
FCLoopScalelessQ[SFAD[{{0, 2 p . q}, 0}, p], {p}]
```

$$\text{True}$$

A 1-loop massive eikonal integral.

```mathematica
FCLoopScalelessQ[SFAD[{{0, 2 p . q}, 0}, p], {p}]
```

$$\text{True}$$

A scaleless topology

```mathematica
FCTopology[topo, {SFAD[{{I p3, 0}, {0, 1}, 1}], SFAD[{{I p1, 0}, {0, 1}, 1}], SFAD[{{0, -2 p1 . q}, {0, 1}, 1}], SFAD[{{I p3 + I q, 0}, {-mb^2, 1}, 1}], SFAD[{{0, p1 . p3}, {0, 1}, 1}]}, {p1, p3}, {q}, {}, {}]
FCLoopScalelessQ[%] 
  
 

```

$$\text{FCTopology}\left(\text{topo},\left\{\frac{1}{(-\text{p3}^2+i \eta )},\frac{1}{(-\text{p1}^2+i \eta )},\frac{1}{(-2 (\text{p1}\cdot q)+i \eta )},\frac{1}{((i \;\text{p3}+i q)^2+\text{mb}^2+i \eta )},\frac{1}{(\text{p1}\cdot \;\text{p3}+i \eta )}\right\},\{\text{p1},\text{p3}\},\{q\},\{\},\{\}\right)$$

$$\text{True}$$
