`FCLoopToPakForm[int, {p1, p2, ...}]` determines a canonical $UF$-based representation for the scalar multi-loop integral `int` that depend on the loop momenta `p1, p2, ...` using the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868).

The current implementation is based on the `FindEquivalents` function from FIRE 6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808). `FCLoopToPakForm` is a backend function used in `FCLoopPakScalelessQ`, `FCLoopFindIntegralMappings`, `FCLoopFindTopologyMappings` etc.

It is also possible to invoke the function as `FCLoopToPakForm[GLI[...], FCTopology[...]]` or FCLoopToPakForm[FCTopology[...]]. Notice that in this case the value of the option `FinalSubstitutions` is ignored, as replacement rules will be extracted directly from the definition of the topology.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakScalelessQ](FCLoopPakScalelessQ.md), [FCLoopScalelessQ](FCLoopScalelessQ.md), [FCLoopFindIntegralMappings](FCLoopFindIntegralMappings.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md).

### Examples

```mathematica
FCLoopToPakForm[FAD[p1, {p3, m1}, {p1 - p4, m1}, p1 + q1, p1 + q1, p3 + q1, p1 - p3 - p4], 
  {p1, p3, p4}, Names -> x, Head -> ph, Power -> pow]
```

$$\left\{\frac{1}{\text{p1}^2.\left(\text{p3}^2-\text{m1}^2\right).\left((\text{p1}-\text{p4})^2-\text{m1}^2\right).(\text{p1}+\text{q1})^4.(\text{p3}+\text{q1})^2.(\text{p1}-\text{p3}-\text{p4})^2},\text{ph}\left(\text{m1}^2 \;\text{pow}(2) x(2) x(4)^2 x(6)+\text{m1}^2 \;\text{pow}(2) x(3) x(4)^2 x(6)+\text{m1}^2 \;\text{pow}(2) x(2)^2 x(3) x(6)+\text{m1}^2 \;\text{pow}(2) x(2)^2 x(4) x(6)+2 \;\text{m1}^2 \;\text{pow}(2) x(2) x(3) x(4) x(6)+\text{m1}^2 \;\text{pow}(2) x(2)^2 x(5) x(6)+\text{m1}^2 \;\text{pow}(2) x(2) x(3) x(5) x(6)+\text{m1}^2 \;\text{pow}(2) x(2) x(4) x(5) x(6)+\text{m1}^2 \;\text{pow}(2) x(3) x(4) x(5) x(6)+\text{m1}^2 x(1) x(2) x(4)^2+\text{m1}^2 x(1) x(3) x(4)^2+\text{m1}^2 x(1) x(2)^2 x(3)+\text{m1}^2 x(1) x(2)^2 x(4)+2 \;\text{m1}^2 x(1) x(2) x(3) x(4)+\text{m1}^2 x(1) x(2)^2 x(5)+\text{m1}^2 x(1) x(2) x(3) x(5)+\text{m1}^2 x(1) x(2) x(4) x(5)+\text{m1}^2 x(1) x(3) x(4) x(5)-\text{pow}(2) \;\text{q1}^2 x(1) x(2) x(3) x(6)-\text{pow}(2) \;\text{q1}^2 x(1) x(2) x(4) x(6)-\text{pow}(2) \;\text{q1}^2 x(1) x(3) x(4) x(6)-\text{pow}(2) \;\text{q1}^2 x(1) x(2) x(5) x(6)-\text{pow}(2) \;\text{q1}^2 x(1) x(3) x(5) x(6)-\text{pow}(2) \;\text{q1}^2 x(2) x(3) x(5) x(6)-\text{pow}(2) \;\text{q1}^2 x(2) x(4) x(5) x(6)-\text{pow}(2) \;\text{q1}^2 x(3) x(4) x(5) x(6)+\text{pow}(2) x(2) x(3) x(6)+\text{pow}(2) x(2) x(4) x(6)+\text{pow}(2) x(3) x(4) x(6)+\text{pow}(2) x(2) x(5) x(6)+\text{pow}(2) x(3) x(5) x(6)-\text{q1}^2 x(1) x(2) x(3) x(5)-\text{q1}^2 x(1) x(2) x(4) x(5)-\text{q1}^2 x(1) x(3) x(4) x(5)+x(1) x(2) x(3)+x(1) x(2) x(4)+x(1) x(3) x(4)+x(1) x(2) x(5)+x(1) x(3) x(5),\left(
\begin{array}{cccccc}
 x(1) & x(3) & x(4) & x(2) & x(6) & x(5) \\
 \frac{1}{\text{p1}^2} & \frac{1}{(\text{p1}-\text{p4})^2-\text{m1}^2} & \frac{1}{(\text{p1}-\text{p3}-\text{p4})^2} & \frac{1}{\text{p3}^2-\text{m1}^2} & \frac{1}{(\text{p3}+\text{q1})^2} & \frac{1}{(\text{p1}+\text{q1})^2} \\
 1 & 1 & 1 & 1 & 1 & 2 \\
\end{array}
\right)\right)\right\}$$

```mathematica
topo1 = FCTopology["prop2Lv1", {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[p1 - q], SFAD[p2 - q], SFAD[{p1 - p2, m3^2}]}, {p1, p2}, {Q}, {}, {}]
topo2 = FCTopology["prop2Lv2", {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[{p1 - q, M^2}], SFAD[{p2 - q, M^2}], SFAD[p1 - p2]}, {p1, p2}, {Q}, {}, {}]
```

$$\text{FCTopology}\left(\text{prop2Lv1},\left\{\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )},\frac{1}{((\text{p1}-q)^2+i \eta )},\frac{1}{((\text{p2}-q)^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2-\text{m3}^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\},\{\}\right)$$

$$\text{FCTopology}\left(\text{prop2Lv2},\left\{\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )},\frac{1}{((\text{p1}-q)^2-M^2+i \eta )},\frac{1}{((\text{p2}-q)^2-M^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\},\{\}\right)$$

```mathematica
FCLoopToPakForm[topo1, Names -> x, Head -> ph, Power -> pow]
```

$$\left\{\text{FCTopology}\left(\text{prop2Lv1},\left\{\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )},\frac{1}{((\text{p1}-q)^2+i \eta )},\frac{1}{((\text{p2}-q)^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2-\text{m3}^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\},\{\}\right),\text{ph}\left(\text{m1}^2 x(1) x(2)^2+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(2)^2 x(4)+\text{m1}^2 x(1) x(2) x(4)+\text{m1}^2 x(2) x(3) x(4)+\text{m1}^2 x(2)^2 x(5)+\text{m1}^2 x(1) x(2) x(5)+\text{m1}^2 x(2) x(3) x(5)+\text{m2}^2 x(1) x(4)^2+\text{m2}^2 x(2) x(4)^2+\text{m2}^2 x(3) x(4)^2+\text{m2}^2 x(1) x(2) x(4)+\text{m2}^2 x(1) x(3) x(4)+\text{m2}^2 x(1) x(4) x(5)+\text{m2}^2 x(2) x(4) x(5)+\text{m2}^2 x(3) x(4) x(5)+\text{m3}^2 x(1)^2 x(2)+\text{m3}^2 x(1)^2 x(3)+\text{m3}^2 x(1)^2 x(4)+\text{m3}^2 x(1) x(2) x(4)+\text{m3}^2 x(1) x(3) x(4)+\text{m3}^2 x(1)^2 x(5)+\text{m3}^2 x(1) x(2) x(5)+\text{m3}^2 x(1) x(3) x(5)-q^2 x(1) x(2) x(3)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4)-q^2 x(1) x(2) x(5)-q^2 x(2) x(3) x(5)-q^2 x(1) x(4) x(5)-q^2 x(2) x(4) x(5)-q^2 x(3) x(4) x(5)+x(1) x(2)+x(1) x(3)+x(1) x(4)+x(2) x(4)+x(3) x(4)+x(1) x(5)+x(2) x(5)+x(3) x(5),\left(
\begin{array}{ccccc}
 x(5) & x(1) & x(3) & x(2) & x(4) \\
 \frac{1}{((\text{p1}-\text{p2})^2-\text{m3}^2+i \eta )} & \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & \frac{1}{((\text{p1}-q)^2+i \eta )} & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & \frac{1}{((\text{p2}-q)^2+i \eta )} \\
 1 & 1 & 1 & 1 & 1 \\
\end{array}
\right)\right)\right\}$$

```mathematica
FCLoopToPakForm[{GLI["prop2Lv1", {1, 1, 1, 1, 0}], GLI["prop2Lv2", {1, 1, 0, 0, 1}]}, {topo1, topo2}, Names -> x, Head -> ph, Power -> pow]
```

$$\left(
\begin{array}{cc}
 G^{\text{prop2Lv1}}(1,1,1,1,0) & \;\text{ph}\left(\text{m1}^2 x(1)^2 x(3)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1)^2 x(4)+\text{m1}^2 x(1) x(2) x(4)+\text{m2}^2 x(1) x(3)^2+\text{m2}^2 x(2) x(3)^2+\text{m2}^2 x(1) x(3) x(4)+\text{m2}^2 x(2) x(3) x(4)-q^2 x(1) x(2) x(3)-q^2 x(1) x(2) x(4)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4)+x(1) x(3)+x(2) x(3)+x(1) x(4)+x(2) x(4),\left(
\begin{array}{cccc}
 x(1) & x(3) & x(2) & x(4) \\
 \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & \frac{1}{((\text{p1}-q)^2+i \eta )} & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & \frac{1}{((\text{p2}-q)^2+i \eta )} \\
 1 & 1 & 1 & 1 \\
\end{array}
\right)\right) \\
 G^{\text{prop2Lv2}}(1,1,0,0,1) & \;\text{ph}\left(\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1)^2 x(3)+\text{m1}^2 x(1) x(2) x(3)+\text{m2}^2 x(1) x(2)^2+\text{m2}^2 x(2)^2 x(3)+\text{m2}^2 x(1) x(2) x(3)+x(1) x(2)+x(1) x(3)+x(2) x(3),\left(
\begin{array}{ccc}
 x(1) & x(3) & x(2) \\
 \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & \frac{1}{((\text{p1}-\text{p2})^2+i \eta )} \\
 1 & 1 & 1 \\
\end{array}
\right)\right) \\
\end{array}
\right)$$

Products of `GLI`s are also supported.

```mathematica
FCLoopToPakForm[{GLI["prop2Lv1", {1, 1, 0, 0, 0}]^2}, {topo1, topo2}, Names -> x, Head -> ph, Power -> pow]
```

$$\left(
\begin{array}{cc}
 G^{\text{prop2Lv1}}(1,1,0,0,0)^2 & \;\text{ph}\left(\text{m1}^2 x(2) x(3) x(4) x(1)^2+\text{m1}^2 x(2)^2 x(3) x(4) x(1)+\text{m2}^2 x(2) x(3) x(4)^2 x(1)+\text{m2}^2 x(2) x(3)^2 x(4) x(1)+x(2) x(3) x(4) x(1),\left(
\begin{array}{cccc}
 x(1) & x(3) & x(2) & x(4) \\
 \frac{1}{(\text{FCGV}(\text{lmom})(1,1)^2-\text{m1}^2+i \eta )} & \frac{1}{(\text{FCGV}(\text{lmom})(2,1)^2-\text{m1}^2+i \eta )} & \frac{1}{(\text{FCGV}(\text{lmom})(1,2)^2-\text{m2}^2+i \eta )} & \frac{1}{(\text{FCGV}(\text{lmom})(2,2)^2-\text{m2}^2+i \eta )} \\
 1 & 1 & 1 & 1 \\
\end{array}
\right)\right) \\
\end{array}
\right)$$