## FCLoopPropagatorsToLineMomenta

`FCLoopPropagatorsToLineMomenta[{prop1, prop2, ...}]` is an auxiliary function that extracts line momenta flowing through the given list of propagators.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopIntegralToGraph](FCLoopIntegralToGraph.md), [AuxiliaryMomenta](AuxiliaryMomenta.md).

### Examples

```mathematica
FCLoopPropagatorsToLineMomenta[{SFAD[{q + l, m^2}], SFAD[{p, -m^2}]}, FCE -> True]
```

$$\left(
\begin{array}{cc}
 l+q & p \\
 -m^2 & m^2 \\
 \frac{1}{((l+q)^2-m^2+i \eta )} & \frac{1}{(p^2+m^2+i \eta )} \\
\end{array}
\right)$$

```mathematica
FCLoopPropagatorsToLineMomenta[{CFAD[{{0, 2 v . (q + r)}, m^2}]}, FCE -> True, AuxiliaryMomenta -> {v}]
```

$$\left(
\begin{array}{c}
 q+r \\
 m^2 \\
 \frac{1}{(2 ((q+r)\cdot v)+m^2-i \eta )} \\
\end{array}
\right)$$

Reversed signs are also supported

```mathematica
{SFAD[{I (q + l), -m^2}], SFAD[{I p, -m^2}]}
FCLoopPropagatorsToLineMomenta[%, FCE -> True]
```

$$\left\{\frac{1}{(-(l+q)^2+m^2+i \eta )},\frac{1}{(-p^2+m^2+i \eta )}\right\}$$

$$\left(
\begin{array}{cc}
 l+q & p \\
 m^2 & m^2 \\
 \frac{1}{(-(l+q)^2+m^2+i \eta )} & \frac{1}{(-p^2+m^2+i \eta )} \\
\end{array}
\right)$$

```mathematica
FCLoopPropagatorsToLineMomenta[{SFAD[{I (q + l), -m^2}], SFAD[{I p, -m^2}]}, FCE -> True] // InputForm
```

```mathematica
{{l + q, p}, {m^2, m^2}, {SFAD[{{I*(l + q), 0}, {-m^2, 1}, 1}], 
  SFAD[{{I*p, 0}, {-m^2, 1}, 1}]}}
```

```mathematica
FCLoopPropagatorsToLineMomenta[{SFAD[{{I p1, -2 p1 . q}, {0, 1}, 1}]},FCE -> True]
```

$$\left(
\begin{array}{c}
 \;\text{p1}+q \\
 0 \\
 \frac{1}{(-\text{p1}^2-2 (\text{p1}\cdot q)+i \eta )} \\
\end{array}
\right)$$
