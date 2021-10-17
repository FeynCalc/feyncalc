`FCLoopSelectTopology[int, topos]` selects the topology that matches the `GLI` `int` from a list of topologies `topos`.

The first argument can be also a list, in which case the function will return a list of matching topologies.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).

### Examples

```mathematica
topos = {FCTopology[topo2, {FAD[{p1, m}], FAD[{p1 - q, m}]}, {p1}, {q}, {}, {}], 
   FCTopology[topo1, {FAD[{p1, m}], FAD[{p1 - q, m}]}, {p1}, {q}, {SPD[q] -> M^2,SPD[q2] -> M2^2}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{\text{p1}^2-m^2},\frac{1}{(\text{p1}-q)^2-m^2}\right\},\{\text{p1}\},\{q\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{\text{p1}^2-m^2},\frac{1}{(\text{p1}-q)^2-m^2}\right\},\{\text{p1}\},\{q\},\left\{q^2\to M^2,\text{q2}^2\to \;\text{M2}^2\right\},\{\}\right)\right\}$$

```mathematica
FCLoopSelectTopology[{GLI[topo2, {2, 2}]}, topos]
```

$$\left\{\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{\text{p1}^2-m^2},\frac{1}{(\text{p1}-q)^2-m^2}\right\},\{\text{p1}\},\{q\},\{\},\{\}\right)\right\}$$

```mathematica
topos = {FCTopology[prop2Ltopo13311, {SFAD[{{I*p1, 0}, {-m1^2, -1}, 1}], 
     SFAD[{{I*(p1 + q1), 0}, {-m3^2, -1}, 1}], SFAD[{{I*p3, 0}, {-m3^2, -1}, 1}], 
     SFAD[{{I*(p3 + q1), 0}, {-m1^2, -1}, 1}], SFAD[{{I*(p1 - p3), 0}, 
       {-m1^2, -1}, 1}]}, {p1, p3}, {q1}, {SPD[q1, q1] -> m1^2}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{prop2Ltopo13311},\left\{\frac{1}{(-\text{p1}^2+\text{m1}^2-i \eta )},\frac{1}{(-(\text{p1}+\text{q1})^2+\text{m3}^2-i \eta )},\frac{1}{(-\text{p3}^2+\text{m3}^2-i \eta )},\frac{1}{(-(\text{p3}+\text{q1})^2+\text{m1}^2-i \eta )},\frac{1}{(-(\text{p1}-\text{p3})^2+\text{m1}^2-i \eta )}\right\},\{\text{p1},\text{p3}\},\{\text{q1}\},\left\{\text{q1}^2\to \;\text{m1}^2\right\},\{\}\right)\right\}$$

Products of `GLI`s in the first argument are also supported.

```mathematica
FCLoopSelectTopology[{GLI[prop2Ltopo13311, {1, 0, 0, 0, 0}]^2, GLI[prop2Ltopo13311, {1, 1, 1, 1, 1}]}, topos, FCE -> True]
```

$$\left\{\text{FCTopology}\left(\text{prop2Ltopo13311},\left\{\frac{1}{(-\text{p1}^2+\text{m1}^2-i \eta )},\frac{1}{(-(\text{p1}+\text{q1})^2+\text{m3}^2-i \eta )},\frac{1}{(-\text{p3}^2+\text{m3}^2-i \eta )},\frac{1}{(-(\text{p3}+\text{q1})^2+\text{m1}^2-i \eta )},\frac{1}{(-(\text{p1}-\text{p3})^2+\text{m1}^2-i \eta )}\right\},\{\text{p1},\text{p3}\},\{\text{q1}\},\left\{\text{q1}^2\to \;\text{m1}^2\right\},\{\}\right)\right\}$$