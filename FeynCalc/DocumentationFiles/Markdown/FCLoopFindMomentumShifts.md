`FCLoopFindMomentumShifts[source, target, {p1, p2, ...}]` finds loop momentum shifts that bring loop integrals or topologies in the list `source` to the form specified in target. The integrals/topologies in `intFrom` and `intTo` are assumed to be equivalent and their denominators must be properly ordered via `FCLoopToPakForm`. Furthermore, `target` must be provided as a list of `FeynAmpDenominator` objects, while `intFrom` is a list of such lists.

### See also

[FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakOrder](FCLoopPakOrder).

### Examples

```mathematica
source = {{FAD[p4], FAD[p1], FAD[p1 - p3 - p4], 
    FAD[{p1 - p4, m1}], FAD[{p3, m1}], FAD[p3 + q1], 
    FAD[p1 + q1]}}
```

$$\left(
\begin{array}{ccccccc}
 \frac{1}{\text{p4}^2} & \frac{1}{\text{p1}^2} & \frac{1}{(\text{p1}-\text{p3}-\text{p4})^2} & \frac{1}{(\text{p1}-\text{p4})^2-\text{m1}^2} & \frac{1}{\text{p3}^2-\text{m1}^2} & \frac{1}{(\text{p3}+\text{q1})^2} & \frac{1}{(\text{p1}+\text{q1})^2} \\
\end{array}
\right)$$

```mathematica
target = {FAD[p4], FAD[p1 + p4 + q1], FAD[p1 - p3 + q1], 
   FAD[{p1 + q1, m1}], FAD[{p3, m1}], FAD[p3 + q1], 
   FAD[p1 + p4 + 2 q1]}
```

$$\left\{\frac{1}{\text{p4}^2},\frac{1}{(\text{p1}+\text{p4}+\text{q1})^2},\frac{1}{(\text{p1}-\text{p3}+\text{q1})^2},\frac{1}{(\text{p1}+\text{q1})^2-\text{m1}^2},\frac{1}{\text{p3}^2-\text{m1}^2},\frac{1}{(\text{p3}+\text{q1})^2},\frac{1}{(\text{p1}+\text{p4}+2 \text{q1})^2}\right\}$$

```mathematica
FCLoopFindMomentumShifts[source, target, {p1, p3, p4}]
```

$$\{\{\text{p1}\to \text{p1}+\text{p4}+\text{q1},\text{p3}\to \text{p3},\text{p4}\to \text{p4}\}\}$$

```mathematica
source = {FCTopology[
    fctopology3, {SFAD[{{p1, 0}, {0, 1}, 1}], 
    	SFAD[{{p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p2, 0}, {0, 1}, 1}]}], 
   FCTopology[
    fctopology4, {SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3, 0}, {0, 1}, 1}]}]}
```

$$\left\{\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )}\right\}\right)\right\}$$

```mathematica
target = FCTopology[
   fctopology1, {SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
    SFAD[{{p3, 0}, {0, 1}, 1}], 
    SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], 
    SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
    SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
    SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
    SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}]}]
```

$$\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )}\right\}\right)$$

```mathematica
FCLoopFindMomentumShifts[source, target, {p1, p2, p3}]
```

$$\{\{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q,\text{p3}\to \text{p3}\},\{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\}\}$$