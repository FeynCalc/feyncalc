## FCLoopFindMomentumShifts

`FCLoopFindMomentumShifts[source, target, {p1, p2, ...}]` finds loop momentum shifts that bring loop integrals or topologies in the list `source` to the form specified in target. The integrals/topologies in `intFrom` and `intTo` are assumed to be equivalent and their denominators must be properly ordered via `FCLoopToPakForm`. Here the loop momenta `p1, p2, ...` belong to the source topologies.

`target` must be provided as a list of `FeynAmpDenominator` objects, while `intFrom` is a list of such lists.

It is also possible to invoke the function as `FCLoopFindMomentumShifts[{FCTopology[...], FCTopology[...]}, FCTopology[...]]`.

For topologies involving kinematic constraints some mappings may require shifts not only in the loop but also in the external momenta. Such shifts are disabled by default but can be activated by setting the option `Momentum` to `All`. This option can be dangerous, because the amplitude does not necessarily have to be symmetric under shifts of external momenta!

Normally, `FCLoopFindMomentumShifts` will abort the evaluation if it fails to find any suitable shifts. Setting the option `Abort` to `False` will force the function to merely return an empty list in such situations.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakOrder](FCLoopPakOrder.md).

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

$$\left\{\frac{1}{\text{p4}^2},\frac{1}{(\text{p1}+\text{p4}+\text{q1})^2},\frac{1}{(\text{p1}-\text{p3}+\text{q1})^2},\frac{1}{(\text{p1}+\text{q1})^2-\text{m1}^2},\frac{1}{\text{p3}^2-\text{m1}^2},\frac{1}{(\text{p3}+\text{q1})^2},\frac{1}{(\text{p1}+\text{p4}+2 \;\text{q1})^2}\right\}$$

```mathematica
FCLoopFindMomentumShifts[source, target, {p1, p3, p4}]
```

$$\{\{\text{p1}\to \;\text{p1}+\text{p4}+\text{q1}\}\}$$

```mathematica
FCLoopFindMomentumShifts[{{FAD[r4], FAD[r1], FAD[r1 - p3 - r4], 
    FAD[{r1 - r4, m1}], FAD[{p3, m1}], FAD[p3 + q1], FAD[r1 + q1]}}, 
  {FAD[p4], FAD[p1 + p4 + q1], FAD[p1 - p3 + q1], FAD[{p1 + q1, m1}], 
   FAD[{p3, m1}], FAD[p3 + q1], FAD[p1 + p4 + 2 q1]}, {p1, p3, p4, r4,r1}]
```

$$\{\{\text{r4}\to \;\text{p4},\text{r1}\to \;\text{p1}+\text{p4}+\text{q1}\}\}$$

```mathematica
source1 = {FCTopology[
    fctopology3, {SFAD[{{p1, 0}, {0, 1}, 1}], 
    	SFAD[{{p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p2, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
   FCTopology[
    fctopology4, {SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p3, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
    	SFAD[{{p1 + p3, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right)\right\}$$

```mathematica
target1 = FCTopology[
   fctopology1, {SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
    SFAD[{{p3, 0}, {0, 1}, 1}], 
    SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], 
    SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
    SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
    SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
    SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}]
```

$$\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right)$$

```mathematica
FCLoopFindMomentumShifts[source1, target1]
```

$$\{\{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q\},\{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\}\}$$

```mathematica
source2 = {FCTopology[topo1, {
     SFAD[{{l1 + q1, 0}, {m^2, 1}, 1}], 
     SFAD[{{l1 - l2, 0}, {0, 1}, 1}], 
     SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
     SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
     SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, 
    {SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{((\text{l1}+\text{q1})^2-m^2+i \eta )},\frac{1}{((\text{l1}-\text{l2})^2+i \eta )},\frac{1}{((\text{l2}+\text{q1})^2-m^2+i \eta )},\frac{1}{((\text{l2}-\text{q2})^2-m^2+i \eta )},\frac{1}{(\text{l2}^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\left\{\text{q1}^2\to 0,\text{q2}^2\to 0,\text{q1}\cdot \;\text{q2}\to \frac{s}{2}\right\},\{\}\right)\right\}$$

```mathematica
target2 = FCTopology[topo2, {
    SFAD[{{l1 - l2, 0}, {m^2, 1}, 1}], 
    SFAD[{{l1 - q2, 0}, {0, 1}, 1}], 
    SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
    SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
    SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, 
   {SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}]
```

$$\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{((\text{l1}-\text{l2})^2-m^2+i \eta )},\frac{1}{((\text{l1}-\text{q2})^2+i \eta )},\frac{1}{((\text{l2}-\text{q2})^2-m^2+i \eta )},\frac{1}{((\text{l2}+\text{q1})^2-m^2+i \eta )},\frac{1}{(\text{l2}^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\left\{\text{q1}^2\to 0,\text{q2}^2\to 0,\text{q1}\cdot \;\text{q2}\to \frac{s}{2}\right\},\{\}\right)$$

Mapping these two topologies onto each other requires shifts in the external momenta

```mathematica
Quiet[FCLoopFindMomentumShifts[source2, target2, Abort -> False]]
```

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies topo1 and topo2. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\{\{\}\}$$

Once we allow such shifts, everything works as expected

```mathematica
FCLoopFindMomentumShifts[source2, target2, Momentum -> All]
```

$$\{\{\text{l1}\to -\text{l1}+\text{l2}+\text{q2},\text{q1}\to -\text{q2},\text{q2}\to -\text{q1}\}\}$$

For equivalent topologies containing mixed quadratic-eikonal propagators it's often not possible to find suitable shifts because the function cannot reconstruct the correct momentum flow through such propagators

```mathematica
source3 = {FCTopology["pfrTopo303", {SFAD[{{k2, -2 gkin meta k2 . n + meta u0b k2 . nb}, 
       {2 gkin meta^2 u0b, 1}, 1}], SFAD[{{k1 - k2, meta u0b (-k1 + k2) . nb}, {0, 1}, 1}], 
     SFAD[{{k1, -2 gkin meta k1 . n}, {0, 1}, 1}], SFAD[{{k1, -meta u0b k1 . nb}, {0, 1}, 1}], 
     SFAD[{{0, -k2 . nb}, {0, 1}, 1}], SFAD[{{0, -k1 . nb}, {0, 1}, 1}]}, {k1, k2}, {n, nb}, 
    {Hold[SPD][n] -> 0, Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{pfrTopo303},\left\{\frac{1}{(\text{k2}^2+\text{meta} \;\text{u0b} (\text{k2}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta} (\text{k2}\cdot n)-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{((\text{k1}-\text{k2})^2+\text{meta} \;\text{u0b} ((\text{k2}-\text{k1})\cdot \;\text{nb})+i \eta )},\frac{1}{(\text{k1}^2-2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k1}^2-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})+i \eta )},\frac{1}{(-\text{k2}\cdot \;\text{nb}+i \eta )},\frac{1}{(-\text{k1}\cdot \;\text{nb}+i \eta )}\right\},\{\text{k1},\text{k2}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right)\right\}$$

```mathematica
target3 = FCTopology["pfrTopo267", {SFAD[{{k2, 0}, {0, 1}, 1}], SFAD[{{k1 - k2, 2 gkin meta k1 . n - 2 gkin meta u0b k1 . n - 2 gkin meta k2 . n + 2 gkin meta u0b k2 . n + meta u0b (-k1 + k2) . nb}, {2 gkin meta^2 u0b - 2 gkin meta^2 u0b^2, 1}, 1}], SFAD[{{k1, 2 gkin meta k1 . n - 2 gkin meta u0b k1 . n - meta u0b k1 . nb}, {2 gkin meta^2 u0b - 2 gkin meta^2 u0b^2, 1}, 1}], SFAD[{{k1, -2 gkin meta u0b k1 . n}, {0, 1}, 1}], SFAD[{{0, k2 . nb}, {2 gkin meta, 1}, 1}], SFAD[{{0, k1 . nb}, {2 gkin meta u0b, 1}, 1}]}, {k1, k2}, {n, nb}, {Hold[SPD][n] -> 0, Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2}, {}]
```

$$\text{FCTopology}\left(\text{pfrTopo267},\left\{\frac{1}{(\text{k2}^2+i \eta )},1\left/\left(((\text{k1}-\text{k2})^2+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)-2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)-2 \;\text{gkin} \;\text{meta} (\text{k2}\cdot n)+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k2}\cdot n)+\text{meta} \;\text{u0b} ((\text{k2}-\text{k1})\cdot \;\text{nb})+2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )\right)\right.,\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)-2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})+2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{(\text{k1}^2-2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k2}\cdot \;\text{nb}-2 \;\text{gkin} \;\text{meta}+i \eta )},\frac{1}{(\text{k1}\cdot \;\text{nb}-2 \;\text{gkin} \;\text{meta} \;\text{u0b}+i \eta )}\right\},\{\text{k1},\text{k2}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right)$$

$$(\text{DataType}[\#,\text{FCVariable}]=\text{True})\&\text{/@}\{\text{gkin},\text{meta},\text{u0b}\};$$

```mathematica
FCLoopFindMomentumShifts[source3, target3]
```

$$\text{FCLoopFindMomentumShifts: }\;\text{The topologies contain following mixed quadratic-eikonal propagators that complicate the determination of the shifts: }\left\{\frac{1}{(\text{k1}^2-2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k1}^2-2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k1}^2-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})+i \eta )},\frac{1}{(\text{k1}^2+n\cdot (2 \;\text{gkin} \;\text{meta} \;\text{k1}-2 \;\text{gkin} \;\text{meta} \;\text{u0b} \;\text{k1})-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})+2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{((\text{k1}-\text{k2})^2+\text{meta} \;\text{u0b} ((\text{k2}-\text{k1})\cdot \;\text{nb})+i \eta )},\frac{1}{((\text{k1}-\text{k2})^2+\text{meta} \;\text{u0b} ((\text{k2}-\text{k1})\cdot \;\text{nb})+n\cdot (2 \;\text{gkin} \;\text{meta} \;\text{k1}-2 \;\text{gkin} \;\text{meta} \;\text{u0b} \;\text{k1}-2 \;\text{gkin} \;\text{meta} \;\text{k2}+2 \;\text{gkin} \;\text{meta} \;\text{u0b} \;\text{k2})+2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{(\text{k2}^2+\text{k2}\cdot (\text{meta} \;\text{u0b} \;\text{nb}-2 \;\text{gkin} \;\text{meta} n)-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )}\right\}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{You can try to trade them for purely quadratic propagators using FCLoopReplaceQuadraticEikonalPropagators.}$$

![189bbth1nyc1e](img/189bbth1nyc1e.svg)

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo303 and pfrTopo267. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{\$Aborted}$$

To this aim one can try converting those mixed propagators to purely quadratic ones using `FCLoopReplaceQuadraticEikonalPropagators`

```mathematica
source3New = FCLoopReplaceQuadraticEikonalPropagators[source3, LoopMomenta -> {k1, k2}, 
   InitialSubstitutions -> {ExpandScalarProduct[SPD[k1 - k2]] -> SPD[k1 - k2]}, 
   IntermediateSubstitutions -> {SPD[n] -> 0, SPD[nb] -> 0, SPD[n, nb] -> 2}]
```

$$\left\{\text{FCTopology}\left(\text{pfrTopo303},\left\{\frac{1}{((\text{k2}-\text{gkin} \;\text{meta} n+\frac{\text{meta} \;\text{u0b} \;\text{nb}}{2})^2+i \eta )},\frac{1}{((\text{k1}-\text{k2}-\frac{\text{meta} \;\text{u0b} \;\text{nb}}{2})^2+i \eta )},\frac{1}{((\text{k1}-\text{gkin} \;\text{meta} n)^2+i \eta )},\frac{1}{((\text{k1}-\frac{\text{meta} \;\text{u0b} \;\text{nb}}{2})^2+i \eta )},\frac{1}{(-\text{k2}\cdot \;\text{nb}+i \eta )},\frac{1}{(-\text{k1}\cdot \;\text{nb}+i \eta )}\right\},\{\text{k1},\text{k2}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right)\right\}$$

```mathematica
target3New = FCLoopReplaceQuadraticEikonalPropagators[target3, LoopMomenta -> {k1, k2}, 
    InitialSubstitutions -> {ExpandScalarProduct[SPD[k1 - k2]] -> SPD[k1 - k2]}, 
    IntermediateSubstitutions -> {SPD[n] -> 0, SPD[nb] -> 0, SPD[n, nb] -> 2}] // First
```

$$\text{FCTopology}\left(\text{pfrTopo267},\left\{\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{((\text{k1}-\text{k2}+\text{gkin} \;\text{meta} n-\text{gkin} \;\text{meta} \;\text{u0b} n-\frac{\text{meta} \;\text{u0b} \;\text{nb}}{2})^2+i \eta )},\frac{1}{((\text{k1}+\text{gkin} \;\text{meta} n-\text{gkin} \;\text{meta} \;\text{u0b} n-\frac{\text{meta} \;\text{u0b} \;\text{nb}}{2})^2+i \eta )},\frac{1}{((\text{k1}-\text{gkin} \;\text{meta} \;\text{u0b} n)^2+i \eta )},\frac{1}{(\text{k2}\cdot \;\text{nb}-2 \;\text{gkin} \;\text{meta}+i \eta )},\frac{1}{(\text{k1}\cdot \;\text{nb}-2 \;\text{gkin} \;\text{meta} \;\text{u0b}+i \eta )}\right\},\{\text{k1},\text{k2}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right)$$

With the new topologies everything works as expected

```mathematica
FCLoopFindMomentumShifts[source3New, target3New]
```

$$\left\{\left\{\text{k1}\to \;\text{gkin} \;\text{meta} n \;\text{u0b}-\text{k1}+\frac{\text{meta} \;\text{nb} \;\text{u0b}}{2},\text{k2}\to \frac{1}{2} (2 \;\text{gkin} \;\text{meta} n-2 \;\text{k2}-\text{meta} \;\text{nb} \;\text{u0b})\right\}\right\}$$