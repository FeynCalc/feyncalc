## FCLoopBasisFindCompletion

`FCLoopBasisFindCompletion[int, {q1, q2, ...}]` determines propagators that need to be included in the loop integral `int` (that depends on the loop momenta `q1`, `q2`, ...), to ensure that the propagators of `int` form a basis.

For integrals with propagators that do not form a basis, such a completion must be found prior to processing those integrals with tools that do Integration-By-Parts (IBP) reduction (e.g. FIRE, KIRA or LiteRed). Furthermore, `int` may not contain linearly dependent propagators.

The input can also consist of an `FCTopology` object or a list thereof.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopBasisIncompleteQ](FCLoopBasisIncompleteQ.md).

### Examples

```mathematica
FAD[q, {q - p + l, m}]
FCLoopBasisFindCompletion[%, {q}]
```

$$\frac{1}{q^2.\left((l-p+q)^2-m^2\right)}$$

$$\left\{\frac{1}{q^2.\left((l-p+q)^2-m^2\right)},\{l\cdot q\}\right\}$$

```mathematica
FAD[{q1, m1}, {q2, m2}]
FCLoopBasisFindCompletion[%, {q1, q2}]
```

$$\frac{1}{\left(\text{q1}^2-\text{m1}^2\right).\left(\text{q2}^2-\text{m2}^2\right)}$$

$$\left\{\frac{1}{\left(\text{q1}^2-\text{m1}^2\right).\left(\text{q2}^2-\text{m2}^2\right)},\{\text{q1}\cdot \;\text{q2}\}\right\}$$

```mathematica
FAD[q1 + p, q2 - k] SPD[q1, q2]
FCLoopBasisFindCompletion[%, {q1, q2}, Method -> {FAD[{q2 + k, m}], FAD[{q1 - p, m}], SPD[p, q2], SPD[k, q1]}]
```

$$\frac{\text{q1}\cdot \;\text{q2}}{(p+\text{q1})^2.(\text{q2}-k)^2}$$

$$\left\{\frac{\text{q1}\cdot \;\text{q2}}{(p+\text{q1})^2.(\text{q2}-k)^2},\left\{\frac{1}{(k+\text{q2})^2-m^2},\frac{1}{(\text{q1}-p)^2-m^2},p\cdot \;\text{q2},k\cdot \;\text{q1}\right\}\right\}$$

Cartesian integrals are also supported.

```mathematica
CFAD[q1, q2, {q1 - l1, m1}, {q2 - l2, m2}]
FCLoopBasisFindCompletion[%, {q1, q2}]
```

$$\frac{1}{(\text{q1}^2-i \eta ).(\text{q2}^2-i \eta ).((\text{q1}-\text{l1})^2+\text{m1}-i \eta ).((\text{q2}-\text{l2})^2+\text{m2}-i \eta )}$$

$$\left\{\frac{1}{(\text{q1}^2-i \eta ).(\text{q2}^2-i \eta ).((\text{q1}-\text{l1})^2+\text{m1}-i \eta ).((\text{q2}-\text{l2})^2+\text{m2}-i \eta )},\{\text{l1}\cdot \;\text{q2},\text{l2}\cdot \;\text{q1},\text{q1}\cdot \;\text{q2}\}\right\}$$

Extending `FCTopology` objects

```mathematica
FCLoopBasisFindCompletion[FCTopology[topo, {FAD[p1], FAD[p2], FAD[p1 - q], FAD[p2 - q]}, {p1, p2}, {q}, {}, {}]]
```

$$\text{FCTopology}\left(\text{topo},\left\{\frac{1}{\text{p1}^2},\frac{1}{\text{p2}^2},\frac{1}{(\text{p1}-q)^2},\frac{1}{(\text{p2}-q)^2},\frac{1}{(\text{p1}\cdot \;\text{p2}+i \eta )}\right\},\{\text{p1},\text{p2}\},\{q\},\{\},\{\}\right)$$

```mathematica
FCLoopBasisFindCompletion[{
   FCTopology[topo1, {FAD[p1], FAD[p2], FAD[p1 - q], FAD[p2 - q]}, {p1, p2}, {q}, {}, {}], 
   FCTopology[topo2, {FAD[p1], FAD[p2], FAD[p1 - q], FAD[p2 - p1]}, {p1, p2}, {q}, {}, {}] 
  }]
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{\text{p1}^2},\frac{1}{\text{p2}^2},\frac{1}{(\text{p1}-q)^2},\frac{1}{(\text{p2}-q)^2},\frac{1}{(\text{p1}\cdot \;\text{p2}+i \eta )}\right\},\{\text{p1},\text{p2}\},\{q\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{\text{p1}^2},\frac{1}{\text{p2}^2},\frac{1}{(\text{p1}-q)^2},\frac{1}{(\text{p2}-\text{p1})^2},\frac{1}{(\text{p2}\cdot q+i \eta )}\right\},\{\text{p1},\text{p2}\},\{q\},\{\},\{\}\right)\right\}$$

The function pays attention to the $i \eta$ signs in the propagators

```mathematica
FCLoopBasisFindCompletion[{FCTopology[
    asyR1prop2Ltopo13011X11011NAux1, {SFAD[{{I*p1, 0}, {0, {-1}}, 1}],
    	SFAD[{{(-I)*p3, 0}, {-mb^2, -1}, 1}], 
    	SFAD[{{I*(p1 + p3), 0}, {-mb^2, -1}, 1}], 
    	SFAD[{{I*(p1 - q), 0}, {-mb^2, -1}, 1}]}, {p1, 
    	p3}, {q}, {SPD[q, q] -> mb^2}, {}]}, FCE -> True]
```

$$\left\{\text{FCTopology}\left(\text{asyR1prop2Ltopo13011X11011NAux1},\left\{\frac{1}{(-\text{p1}^2-i \eta )},\frac{1}{(-\text{p3}^2+\text{mb}^2-i \eta )},\frac{1}{(-(\text{p1}+\text{p3})^2+\text{mb}^2-i \eta )},\frac{1}{(-(\text{p1}-q)^2+\text{mb}^2-i \eta )},\frac{1}{(\text{p3}\cdot q-i \eta )}\right\},\{\text{p1},\text{p3}\},\{q\},\left\{q^2\to \;\text{mb}^2\right\},\{\}\right)\right\}$$