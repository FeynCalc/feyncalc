## FCLoopPropagatorsToTopology

`FCLoopPropagatorsToTopology[{prop1, prop2, ...}]` takes a list of `Pair`s and `FeynAmpDenominator`s and converts it into a list of propagators that can be used to describe a topology.

The input can also consist of an `FCTopology` object or a list thereof.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopIntegralToPropagators](FCLoopIntegralToPropagators.md).

### Examples

```mathematica
{FAD[q]} 
 
FCLoopPropagatorsToTopology[%]
```

$$\left\{\frac{1}{q^2}\right\}$$

$$\left\{q^2\right\}$$

```mathematica
{FAD[{q, m}]} 
 
FCLoopPropagatorsToTopology[%]
```

$$\left\{\frac{1}{q^2-m^2}\right\}$$

$$\left\{q^2-m^2\right\}$$

```mathematica
{FAD[{q, m}], SPD[q, p]} 
 
FCLoopPropagatorsToTopology[%]
```

$$\left\{\frac{1}{q^2-m^2},p\cdot q\right\}$$

$$\left\{q^2-m^2,p\cdot q\right\}$$

```mathematica
FCLoopPropagatorsToTopology[{FCTopology[topo1, {SFAD[{{p1, 0}, {0, 1}, 1}], 
     SFAD[{{p3, 0}, {mb^2, 1}, 1}], SFAD[{{p1 + p3, 0}, {mb^2, 1}, 1}], SFAD[{{p1 - q, 0}, 
       {mb^2, 1}, 1}], SFAD[{{0, p3 . q}, {0, 1}, 1}]},  {p1, p3}, {q}, {}, {}], 
   FCTopology[topo1, {SFAD[{{p1, 0}, {mb^2, 1}, 1}], SFAD[{{p3, 0}, {mb^2, 1}, 1}], 
     SFAD[{{p1 + p3, 0}, {mb^2, 1}, 1}], SFAD[{{p1 - q, 0}, {mb^2, 1}, 1}], 
     SFAD[{{0, (p3 + p1) . q}, {0, 1}, 1}]},  {p1, p3}, {q}, {}, {}]}]
```

$$\left(
\begin{array}{ccccc}
 \;\text{p1}^2 & \;\text{p3}^2-\text{mb}^2 & (\text{p1}+\text{p3})^2-\text{mb}^2 & (\text{p1}-q)^2-\text{mb}^2 & \;\text{p3}\cdot q \\
 \;\text{p1}^2-\text{mb}^2 & \;\text{p3}^2-\text{mb}^2 & (\text{p1}+\text{p3})^2-\text{mb}^2 & (\text{p1}-q)^2-\text{mb}^2 & (\text{p1}+\text{p3})\cdot q \\
\end{array}
\right)$$