## FCLoopApplyTopologyMappings

`FCLoopApplyTopologyMappings[expr, {mappings, topos}]` applies mappings between topologies obtained using `FCLoopFindTopologyMappings` to the output of `FCLoopFindTopologies` denoted as `expr`. The argument `topos` denotes the final set of topologies present in the expression.

Instead of `{mappings, topos}` one can directly use the output `FCLoopFindTopologyMappings`.

By default the function will attempt to rewrite all the occurring loop integrals as `GLI`s. If you just want to apply the mappings without touching the remaining scalar products, 
set the option `FCLoopCreateRulesToGLI` to `False`. Even when all scalar products depending on loop momenta are rewritten as `GLI`s, you can still suppress the step of multiplying out products
of `GLI`s by setting the option `GLIMultiply` to `False`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md).

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
      SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], {p1 -> -p1 - p3 + Q, p2 -> -p2 - p3 + Q, p3 -> p3}, 
    GLI[fctopology3, {n1_, n7_, n8_, n5_, n6_, n4_, n2_, n3_, n9_}] :>
     GLI[fctopology1, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}, 
   
   {FCTopology[fctopology4, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], {p1 -> -p2 + Q, p2 -> -p1 + Q, p3 -> -p3}, 
    GLI[fctopology4, {n1_, n6_, n5_, n8_, n7_, n3_, n2_, n4_, n9_}] :>
     GLI[fctopology1, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}, 
   
   {FCTopology[fctopology5, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}],SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], {p1 -> p2, p2 -> p1, p3 -> p3}, 
    GLI[fctopology5, {n1_, n3_, n2_, n4_, n6_, n5_, n7_, n8_, n9_}] :>
     GLI[fctopology2, {n1, n2, n3, n4, n5, n6, n7, n8, n9}]}}
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q,\text{p3}\to \;\text{p3}\} & G^{\text{fctopology3}}(\text{n1$\_$},\text{n7$\_$},\text{n8$\_$},\text{n5$\_$},\text{n6$\_$},\text{n4$\_$},\text{n2$\_$},\text{n3$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\} & G^{\text{fctopology4}}(\text{n1$\_$},\text{n6$\_$},\text{n5$\_$},\text{n8$\_$},\text{n7$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology5},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1},\text{p3}\to \;\text{p3}\} & G^{\text{fctopology5}}(\text{n1$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n6$\_$},\text{n5$\_$},\text{n7$\_$},\text{n8$\_$},\text{n9$\_$}):\to G^{\text{fctopology2}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
\end{array}
\right)$$

These are the two topologies onto which everything is mapped

```mathematica
finalTopos = {
   FCTopology[fctopology1, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
   FCTopology[fctopology2, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right)\right\}$$

`FCLoopApplyTopologyMappings`  applies the given mappings to the expression creating an output that is ready to be processed further

```mathematica
FCLoopApplyTopologyMappings[ex, {mappings, finalTopos}, Head -> gliProduct, FCVerbose -> 0]
```

$$\frac{1}{2} G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1) \left(\text{cc1} Q^2+\text{cc4} Q^2+2 \;\text{cc6}\right)+\frac{1}{2} (\text{cc1}-\text{cc4}) G^{\text{fctopology1}}(1,1,0,1,1,1,1,1,1)-\frac{1}{2} (\text{cc1}-\text{cc4}) G^{\text{fctopology1}}(1,1,1,1,1,0,1,1,1)+\frac{1}{2} Q^2 (\text{cc2}+\text{cc5}) G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)-\frac{1}{2} (\text{cc2}+\text{cc5}) G^{\text{fctopology2}}(1,1,1,1,1,0,1,1,1)-\frac{1}{2} \;\text{cc2} G^{\text{fctopology2}}(1,1,1,1,0,1,1,1,1)+\frac{1}{2} \;\text{cc2} G^{\text{fctopology2}}(1,1,1,1,1,1,1,0,1)+\text{cc3} G^{\text{fctopology1}}(1,1,1,1,1,1,0,1,1)+\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(0,1,1,1,1,1,1,1,1)-\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(1,1,1,0,1,1,1,1,1)-\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(1,1,1,1,1,1,1,0,1)+\frac{1}{2} \;\text{cc4} G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,0)+\frac{1}{2} \;\text{cc5} G^{\text{fctopology2}}(1,1,0,1,1,1,1,1,1)$$

This just applies the mappings without any further simplifications

```mathematica
FCLoopApplyTopologyMappings[ex, {mappings, finalTopos}, Head -> gliProduct, FCLoopCreateRulesToGLI -> False]
```

$$\text{gliProduct}\left(\text{cc3} (-\text{p2}-\text{p3}+Q)^2,G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc4} ((Q-\text{p1})\cdot (Q-\text{p2})),G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc1} (\text{p1}\cdot Q),G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc2} (\text{p1}\cdot \;\text{p2}),G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc5} (\text{p1}\cdot Q),G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc6} \;\text{p1}^2,G^{\text{fctopology1}}(1,1,2,1,1,1,1,1,1)\right)$$

This applies the mappings and eliminates the numerators but still keeps products of `GLI`s in the expression

```mathematica
FCLoopApplyTopologyMappings[ex, {mappings, finalTopos}, Head -> gliProduct, FCLoopCreateRulesToGLI -> True, GLIMultiply -> False]
```

$$\text{gliProduct}\left(\frac{1}{2} \;\text{cc1} \left(G^{\text{fctopology1}}(0,0,-1,0,0,0,0,0,0)-G^{\text{fctopology1}}(0,0,0,0,0,-1,0,0,0)+Q^2\right),G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\frac{1}{2} \;\text{cc2} \left(-G^{\text{fctopology2}}(0,0,0,0,-1,0,0,0,0)-G^{\text{fctopology2}}(0,0,0,0,0,-1,0,0,0)+G^{\text{fctopology2}}(0,0,0,0,0,0,0,-1,0)+Q^2\right),G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc3} G^{\text{fctopology1}}(0,0,0,0,0,0,-1,0,0),G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc4} \left(\frac{1}{2} \left(-G^{\text{fctopology1}}(0,-1,0,0,0,0,0,0,0)+G^{\text{fctopology1}}(0,0,0,0,-1,0,0,0,0)-Q^2\right)+\frac{1}{2} \left(-G^{\text{fctopology1}}(0,0,-1,0,0,0,0,0,0)+G^{\text{fctopology1}}(0,0,0,0,0,-1,0,0,0)-Q^2\right)+\frac{1}{2} \left(G^{\text{fctopology1}}(-1,0,0,0,0,0,0,0,0)+G^{\text{fctopology1}}(0,-1,0,0,0,0,0,0,0)-G^{\text{fctopology1}}(0,0,0,-1,0,0,0,0,0)-G^{\text{fctopology1}}(0,0,0,0,-1,0,0,0,0)-G^{\text{fctopology1}}(0,0,0,0,0,0,0,-1,0)+G^{\text{fctopology1}}(0,0,0,0,0,0,0,0,-1)+Q^2\right)+Q^2\right),G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\frac{1}{2} \;\text{cc5} \left(G^{\text{fctopology2}}(0,0,-1,0,0,0,0,0,0)-G^{\text{fctopology2}}(0,0,0,0,0,-1,0,0,0)+Q^2\right),G^{\text{fctopology2}}(1,1,1,1,1,1,1,1,1)\right)+\text{gliProduct}\left(\text{cc6} G^{\text{fctopology1}}(0,0,-1,0,0,0,0,0,0),G^{\text{fctopology1}}(1,1,2,1,1,1,1,1,1)\right)$$