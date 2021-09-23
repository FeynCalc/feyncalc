## FCLoopApplyTopologyMappings

`FCLoopApplyTopologyMappings[expr, mappings]` applies mappings between topologies obtained using `FCFindTopologyMappings` to the output of `FCFindTopologies` denoted as `expr`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCFindTopologies](FCFindTopologies.md), [FCFindTopologyMappings](FCFindTopologyMappings.md).

### Examples

This is a trial expression representing some loop amplitude that has already been processed using `FCFindTopologies`

```mathematica
ex = gliProduct[cc6*SPD[p1, p1], GLI[fctopology1, {1, 1, 2, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc2*SPD[p1, p2], GLI[fctopology2, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc4*SPD[p1, p2], GLI[fctopology4, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc1*SPD[p1, Q], GLI[fctopology1, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc3*SPD[p2, p2], GLI[fctopology3, {1, 1, 1, 1, 1, 1, 1, 1, 1}]] + 
   gliProduct[cc5*SPD[p2, Q], GLI[fctopology5, {1, 1, 1, 1, 1, 1, 1, 1, 1}]]
```

$$\text{gliProduct}\left(\text{cc1} (\text{p1}\cdot Q),G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc2} (\text{p1}\cdot \;\text{p2}),G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc3} \;\text{p2}^2,G^{\text{fctopology3}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc4} (\text{p1}\cdot \;\text{p2}),G^{\text{fctopology4}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc5} (\text{p2}\cdot Q),G^{\text{fctopology5}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc6} \;\text{p1}^2,G^{\text{fctopology1}}(1,1,2,1,1,1,1,1,1)\right)$$

These mapping rules describe how the 3 topologies "fctopology3", "fctopology4" and "fctopology5" are mapped to the topologies "fctopology1" and "fctopology2"

```mathematica
mappings = {
   {FCTopology[fctopology3, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
      SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}], {p1 -> -p1 - p3 + Q,p2 -> -p2 - p3 + Q, p3 -> p3}, 
    GLI[fctopology3, {n1_, n7_, n8_, n5_, n6_, n4_, n2_, n3_, n9_}] :>GLI[fctopology1, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}, 
   
   {FCTopology[fctopology4, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}], {p1 -> -p2 + Q, p2 -> -p1 + Q, p3 -> -p3}, 
    GLI[fctopology4, {n1_, n6_, n5_, n8_, n7_, n3_, n2_, n4_, n9_}] :>GLI[fctopology1, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}, 
   
   {FCTopology[fctopology5, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}],SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}], {p1 -> p2, p2 -> p1, p3 -> p3}, 
    GLI[fctopology5, {n1_, n3_, n2_, n4_, n6_, n5_, n7_, n8_, n9_}] :>GLI[fctopology2, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}}
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\}\right) & \{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q,\text{p3}\to \;\text{p3}\} & G^{\text{fctopology3}}(\text{n1$\_$},\text{n7$\_$},\text{n8$\_$},\text{n5$\_$},\text{n6$\_$},\text{n4$\_$},\text{n2$\_$},\text{n3$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\}\right) & \{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\} & G^{\text{fctopology4}}(\text{n1$\_$},\text{n6$\_$},\text{n5$\_$},\text{n8$\_$},\text{n7$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology5},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\}\right) & \{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1},\text{p3}\to \;\text{p3}\} & G^{\text{fctopology5}}(\text{n1$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n6$\_$},\text{n5$\_$},\text{n7$\_$},\text{n8$\_$},\text{n9$\_$}):\to G^{\text{fctopology2}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
\end{array}
\right)$$

`FCLoopApplyTopologyMappings`  applies the given mappings to the expression creating an output that is ready to be processed further.

```mathematica
FCLoopApplyTopologyMappings[ex, mappings, Head -> gliProduct]
```

$$\text{cc3} G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1) (-\text{p2}-\text{p3}+Q)^2+\text{cc4} G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1) ((Q-\text{p1})\cdot (Q-\text{p2}))+\text{cc1} G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1) (\text{p1}\cdot Q)+\text{cc2} G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1) (\text{p1}\cdot \;\text{p2})+\text{cc5} G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1) (\text{p1}\cdot Q)+\text{cc6} \;\text{p1}^2 G^{\text{fctopology1}}(1,1,2,1,1,1,1,1,1)$$
