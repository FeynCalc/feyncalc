## FCLoopCreateRuleGLIToGLI

`FCLoopCreateRuleGLIToGLI[topology1, topology2]` creates a GLI replacement rule assuming that the `topology2` is a subtopology of `topology1`. Both topologies must be given as `FCTopology` objects.

It is also possible to use `FCLoopCreateRuleGLIToGLI[topo1, {subtopo1, subtopo2, ...}]` provided that `{subtopo1, subtopo2, ...}` are subtopologies of `topo1` that were obtained by removing some propagators from `topo1` and not performing any loop momentum shifts afterwards.

Furthermore, when working with lists of topologies one can write `FCLoopCreateRuleGLIToGLI[{topo1, topo2, ...}, {{subtopo11, subtopo12, ...}, {subtopo21, subtopo22, ...}, ..}]`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCFindTopologies](FCFindTopologies.md), [FCFindTopologyMappings](FCFindTopologyMappings.md).

### Examples

```mathematica
FCLoopCreateRuleGLIToGLI[FCTopology[topo1, {SFAD[p]}], FCTopology[topo2, {SFAD[p]}]]
```

$$G^{\text{topo2}}(\text{n1$\_$}):\to G^{\text{topo1}}(\text{n1})$$

```mathematica
FCLoopCreateRuleGLIToGLI[FCTopology[topo1, {SFAD[p], SFAD[q]}], 
  FCTopology[topo2, {SFAD[p]}]]
```

$$G^{\text{topo2}}(\text{n1$\_$}):\to G^{\text{topo1}}(\text{n1},0)$$

```mathematica
FCLoopCreateRuleGLIToGLI[FCTopology[topo1, {SFAD[p], SFAD[q]}], 
  FCTopology[topo2, {SFAD[q], SFAD[p]}]]
```

$$G^{\text{topo2}}(\text{n2$\_$},\text{n1$\_$}):\to G^{\text{topo1}}(\text{n1},\text{n2})$$

```mathematica
FCLoopCreateRuleGLIToGLI[FCTopology[topo1, {SFAD[r], SFAD[p], SFAD[q]}], 
  FCTopology[topo2, {SFAD[p]}]]
```

$$G^{\text{topo2}}(\text{n2$\_$}):\to G^{\text{topo1}}(0,\text{n2},0)$$

```mathematica
FCLoopCreateRuleGLIToGLI[FCTopology["tmpTopo4", 
   {SFAD[{{0, (k1 + k2) . nb}, {0, 1}, 1}], SFAD[{{0, (k1 - k3) . n}, {0, 1}, 1}], 
    SFAD[{{0, n . (-k1 - k2 + q)}, {0, 1}, 1}], SFAD[{{0, nb . (-k1 + k3 + q)}, {0, 1}, 1}], 
    SFAD[{{-k1, 0}, {0, 1}, 1}], SFAD[{{k2, 0}, {0, 1}, 1}], SFAD[{{k1 + k2, 0}, {0, 1}, 1}], 
    SFAD[{{-k3, 0}, {0, 1}, 1}], SFAD[{{-k1 + k3, 0}, {0, 1}, 1}], 
    SFAD[{{k1 - k3 - q, 0}, {0, 1}, 1}], SFAD[{{k1 + k2 - k3 - q, 0}, {0, 1}, 1}], 
    SFAD[{{-k1 - k2 + q, 0}, {0, 1}, 1}]}], 
  
  FCTopology["tmpTopo18", {SFAD[{{0, (k1 + k2) . nb}, {0, 1}, 1}], 
    SFAD[{{0, n . (-k1 - k2 + q)}, {0, 1}, 1}], SFAD[{{0, nb . (-k1 + k3 + q)}, {0, 1}, 1}], 
    SFAD[{{-k1, 0}, {0, 1}, 1}], SFAD[{{k2, 0}, {0, 1}, 1}], 
    SFAD[{{k1 + k2, 0}, {0, 1}, 1}], SFAD[{{-k3, 0}, {0, 1}, 1}], 
    SFAD[{{-k1 + k3, 0}, {0, 1}, 1}], SFAD[{{k1 - k3 - q, 0}, {0, 1}, 1}], 
    SFAD[{{k1 + k2 - k3 - q, 0}, {0, 1}, 1}], SFAD[{{-k1 - k2 + q, 0}, {0, 1}, 1}]}]]
```

$$G^{\text{tmpTopo18}}(\text{n1$\_$},\text{n3$\_$},\text{n4$\_$},\text{n5$\_$},\text{n6$\_$},\text{n7$\_$},\text{n8$\_$},\text{n9$\_$},\text{n10$\_$},\text{n11$\_$},\text{n12$\_$}):\to G^{\text{tmpTopo4}}(\text{n1},0,\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9},\text{n10},\text{n11},\text{n12})$$

```mathematica
FCLoopIntegralToGraph[FCTopology["tad2l", {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]]
```

$$\left\{\{1\to 2,1\to 2,1\to 2\},\left(
\begin{array}{ccc}
 \;\text{p2} & 1 & -\text{m2}^2 \\
 \;\text{p1} & 1 & -\text{m1}^2 \\
 \;\text{p1}-\text{p2} & 1 & -\text{m3}^2 \\
\end{array}
\right),\left\{\frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2-\text{m3}^2+i \eta )}\right\},1\right\}$$

```mathematica
FCLoopCreateRuleGLIToGLI[
  {FCTopology["prop2l", {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - q, m3}], FAD[{p1 - q, m4}], FAD[{p1 - p2, m5}]}, {p1, p2}, {q}, {}, {}], 
   FCTopology["tad2l", {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]}, {
   {
    FCTopology["prop2lX1", {FAD[{p2, m2}], FAD[{p1 - q, m3}], FAD[{p1 - q, m4}], FAD[{p1 - p2, m5}]}, {p1, p2}, {q}, {}, {}], 
    FCTopology["prop2lX5", {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - q, m3}], FAD[{p1 - q, m4}]}, {p1, p2}, {q}, {}, {}] 
   }, 
   {
    FCTopology["tad2lX2", {FAD[{p1, m1}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}], 
    FCTopology["tad2lX3", {FAD[{p1, m1}], FAD[{p2, m2}]}, {p1, p2}, {}, {}, {}] 
   } 
  }]
```

$$\left\{\left\{G^{\text{prop2lX1}}(\text{n2$\_$},\text{n3$\_$},\text{n4$\_$},\text{n5$\_$}):\to G^{\text{prop2l}}(0,\text{n2},\text{n3},\text{n4},\text{n5}),G^{\text{prop2lX5}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$},\text{n4$\_$}):\to G^{\text{prop2l}}(\text{n1},\text{n2},\text{n3},\text{n4},0)\right\},\left\{G^{\text{tad2lX2}}(\text{n1$\_$},\text{n3$\_$}):\to G^{\text{tad2l}}(\text{n1},0,\text{n3}),G^{\text{tad2lX3}}(\text{n1$\_$},\text{n2$\_$}):\to G^{\text{tad2l}}(\text{n1},\text{n2},0)\right\}\right\}$$
