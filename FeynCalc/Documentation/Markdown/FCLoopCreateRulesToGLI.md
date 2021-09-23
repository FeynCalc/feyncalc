## FCLoopCreateRulesToGLI

`FCLoopCreateRulesToGLI[topo]` creates replacement rules for converting numerators from the given topology to GLI objects with inverse propagators.

It is also possible to use `FCLoopCreateRulesToGLI[{topo1, topo2, ...}]`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCFindTopologies](FCFindTopologies.md), [FCFindTopologyMappings](FCFindTopologyMappings.md).

### Examples

1-loop tadpole

```mathematica
FCLoopCreateRulesToGLI[FCTopology[topo1, {SFAD[{p1, m^2}]}, {p1}, {}, {}, {}]]
```

$$\left\{\text{p1}^2\to G^{\text{topo1}}(-1)+m^2\right\}$$

2-loop tadpole with 3 different masses

```mathematica
FCLoopCreateRulesToGLI[FCTopology[topo1, {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[{p1 - p2, m3^2}]}, {p1, p2}, {}, {}, {}]]
```

$$\left\{\text{p1}^2\to G^{\text{topo1}}(-1,0,0)+\text{m1}^2,\text{p2}^2\to G^{\text{topo1}}(0,-1,0)+\text{m2}^2,\text{p1}\cdot \;\text{p2}\to \frac{1}{2} \left(G^{\text{topo1}}(-1,0,0)+G^{\text{topo1}}(0,-1,0)-G^{\text{topo1}}(0,0,-1)+\text{m1}^2+\text{m2}^2-\text{m3}^2\right)\right\}$$

2-loop self-energy

```mathematica
FCLoopCreateRulesToGLI[FCTopology["prop2Lv1", 
   {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[p1 - q], SFAD[p2 - q], SFAD[{p1 - p2, m3^2}]}, {p1, p2}, {Q}, {}, {}]]
```

$$\left\{\text{p1}^2\to G^{\text{prop2Lv1}}(-1,0,0,0,0)+\text{m1}^2,\text{p2}^2\to G^{\text{prop2Lv1}}(0,-1,0,0,0)+\text{m2}^2,\text{p1}\cdot \;\text{p2}\to \frac{1}{2} \left(G^{\text{prop2Lv1}}(-1,0,0,0,0)+G^{\text{prop2Lv1}}(0,-1,0,0,0)-G^{\text{prop2Lv1}}(0,0,0,0,-1)+\text{m1}^2+\text{m2}^2-\text{m3}^2\right),\text{p1}\cdot q\to \frac{1}{2} \left(G^{\text{prop2Lv1}}(-1,0,0,0,0)-G^{\text{prop2Lv1}}(0,0,-1,0,0)+\text{m1}^2+q^2\right),\text{p2}\cdot q\to \frac{1}{2} \left(G^{\text{prop2Lv1}}(0,-1,0,0,0)-G^{\text{prop2Lv1}}(0,0,0,-1,0)+\text{m2}^2+q^2\right)\right\}$$

A list of 3-loop self-energy topologies

```mathematica
topoList = {
   FCTopology["prop3Lv1", {SFAD[p1], SFAD[p2], SFAD[p3], SFAD[p1 - p2], SFAD[p2 - p3], SFAD[p1 + q1], 
    	SFAD[p2 + q1], SFAD[p3 + q1], SFAD[{{0, p1 . p3}}]}, {p1, p2, p3}, {q1}, {}, {}], 
   
   FCTopology["prop3L2", {SFAD[p1], SFAD[p2], SFAD[p3], SFAD[p1 - p2], SFAD[p2 - p3], SFAD[p1 - p3], 
    	SFAD[p1 + q1], SFAD[p3 + q1], SFAD[{{0, (p1 - p2) . q1}}]}, {p1, p2, p3}, {q1}, {}, {}], 
  	
   FCTopology["prop3L3", {SFAD[p1], SFAD[p1 - p4], SFAD[p3], SFAD[p4], SFAD[p1 - p3 - p4], SFAD[p1 + q1], 
    	SFAD[p3 + p4 + q1], SFAD[p3 + q1], SFAD[{{0, (p4) . q1}}]}, {p1, p3, p4}, {q1}, {}, {}]	
  }
```

$$\left\{\text{FCTopology}\left(\text{prop3Lv1},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2+i \eta )},\frac{1}{((\text{p2}-\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{q1})^2+i \eta )},\frac{1}{((\text{p2}+\text{q1})^2+i \eta )},\frac{1}{((\text{p3}+\text{q1})^2+i \eta )},\frac{1}{(\text{p1}\cdot \;\text{p3}+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{\text{q1}\},\{\},\{\}\right),\text{FCTopology}\left(\text{prop3L2},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2+i \eta )},\frac{1}{((\text{p2}-\text{p3})^2+i \eta )},\frac{1}{((\text{p1}-\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{q1})^2+i \eta )},\frac{1}{((\text{p3}+\text{q1})^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})\cdot \;\text{q1}+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{\text{q1}\},\{\},\{\}\right),\text{FCTopology}\left(\text{prop3L3},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p4})^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p4}^2+i \eta )},\frac{1}{((\text{p1}-\text{p3}-\text{p4})^2+i \eta )},\frac{1}{((\text{p1}+\text{q1})^2+i \eta )},\frac{1}{((\text{p3}+\text{p4}+\text{q1})^2+i \eta )},\frac{1}{((\text{p3}+\text{q1})^2+i \eta )},\frac{1}{(\text{p4}\cdot \;\text{q1}+i \eta )}\right\},\{\text{p1},\text{p3},\text{p4}\},\{\text{q1}\},\{\},\{\}\right)\right\}$$

```mathematica
FCLoopCreateRulesToGLI[topoList]
```

$$\left\{\left\{\text{p1}^2\to G^{\text{prop3Lv1}}(-1,0,0,0,0,0,0,0,0),\text{p2}^2\to G^{\text{prop3Lv1}}(0,-1,0,0,0,0,0,0,0),\text{p3}^2\to G^{\text{prop3Lv1}}(0,0,-1,0,0,0,0,0,0),\text{p1}\cdot \;\text{p2}\to \frac{1}{2} \left(G^{\text{prop3Lv1}}(-1,0,0,0,0,0,0,0,0)+G^{\text{prop3Lv1}}(0,-1,0,0,0,0,0,0,0)-G^{\text{prop3Lv1}}(0,0,0,-1,0,0,0,0,0)\right),\text{p1}\cdot \;\text{p3}\to G^{\text{prop3Lv1}}(0,0,0,0,0,0,0,0,-1),\text{p2}\cdot \;\text{p3}\to \frac{1}{2} \left(G^{\text{prop3Lv1}}(0,-1,0,0,0,0,0,0,0)+G^{\text{prop3Lv1}}(0,0,-1,0,0,0,0,0,0)-G^{\text{prop3Lv1}}(0,0,0,0,-1,0,0,0,0)\right),\text{p1}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3Lv1}}(-1,0,0,0,0,0,0,0,0)+G^{\text{prop3Lv1}}(0,0,0,0,0,-1,0,0,0)-\text{q1}^2\right),\text{p2}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3Lv1}}(0,-1,0,0,0,0,0,0,0)+G^{\text{prop3Lv1}}(0,0,0,0,0,0,-1,0,0)-\text{q1}^2\right),\text{p3}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3Lv1}}(0,0,-1,0,0,0,0,0,0)+G^{\text{prop3Lv1}}(0,0,0,0,0,0,0,-1,0)-\text{q1}^2\right)\right\},\left\{\text{p1}^2\to G^{\text{prop3L2}}(-1,0,0,0,0,0,0,0,0),\text{p2}^2\to G^{\text{prop3L2}}(0,-1,0,0,0,0,0,0,0),\text{p3}^2\to G^{\text{prop3L2}}(0,0,-1,0,0,0,0,0,0),\text{p1}\cdot \;\text{p2}\to \frac{1}{2} \left(G^{\text{prop3L2}}(-1,0,0,0,0,0,0,0,0)+G^{\text{prop3L2}}(0,-1,0,0,0,0,0,0,0)-G^{\text{prop3L2}}(0,0,0,-1,0,0,0,0,0)\right),\text{p1}\cdot \;\text{p3}\to \frac{1}{2} \left(G^{\text{prop3L2}}(-1,0,0,0,0,0,0,0,0)+G^{\text{prop3L2}}(0,0,-1,0,0,0,0,0,0)-G^{\text{prop3L2}}(0,0,0,0,0,-1,0,0,0)\right),\text{p2}\cdot \;\text{p3}\to \frac{1}{2} \left(G^{\text{prop3L2}}(0,-1,0,0,0,0,0,0,0)+G^{\text{prop3L2}}(0,0,-1,0,0,0,0,0,0)-G^{\text{prop3L2}}(0,0,0,0,-1,0,0,0,0)\right),\text{p1}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3L2}}(-1,0,0,0,0,0,0,0,0)+G^{\text{prop3L2}}(0,0,0,0,0,0,-1,0,0)-\text{q1}^2\right),\text{p2}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3L2}}(-1,0,0,0,0,0,0,0,0)+G^{\text{prop3L2}}(0,0,0,0,0,0,-1,0,0)-2 G^{\text{prop3L2}}(0,0,0,0,0,0,0,0,-1)-\text{q1}^2\right),\text{p3}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3L2}}(0,0,-1,0,0,0,0,0,0)+G^{\text{prop3L2}}(0,0,0,0,0,0,0,-1,0)-\text{q1}^2\right)\right\},\left\{\text{p1}^2\to G^{\text{prop3L3}}(-1,0,0,0,0,0,0,0,0),\text{p3}^2\to G^{\text{prop3L3}}(0,0,-1,0,0,0,0,0,0),\text{p4}^2\to G^{\text{prop3L3}}(0,0,0,-1,0,0,0,0,0),\text{p1}\cdot \;\text{p3}\to \frac{1}{2} \left(G^{\text{prop3L3}}(0,-1,0,0,0,0,0,0,0)+G^{\text{prop3L3}}(0,0,-1,0,0,0,0,0,0)-G^{\text{prop3L3}}(0,0,0,-1,0,0,0,0,0)-G^{\text{prop3L3}}(0,0,0,0,-1,0,0,0,0)+G^{\text{prop3L3}}(0,0,0,0,0,0,-1,0,0)-G^{\text{prop3L3}}(0,0,0,0,0,0,0,-1,0)-2 G^{\text{prop3L3}}(0,0,0,0,0,0,0,0,-1)\right),\text{p1}\cdot \;\text{p4}\to \frac{1}{2} \left(G^{\text{prop3L3}}(-1,0,0,0,0,0,0,0,0)-G^{\text{prop3L3}}(0,-1,0,0,0,0,0,0,0)+G^{\text{prop3L3}}(0,0,0,-1,0,0,0,0,0)\right),\text{p3}\cdot \;\text{p4}\to \frac{1}{2} \left(-G^{\text{prop3L3}}(0,0,0,-1,0,0,0,0,0)+G^{\text{prop3L3}}(0,0,0,0,0,0,-1,0,0)-G^{\text{prop3L3}}(0,0,0,0,0,0,0,-1,0)-2 G^{\text{prop3L3}}(0,0,0,0,0,0,0,0,-1)\right),\text{p1}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3L3}}(-1,0,0,0,0,0,0,0,0)+G^{\text{prop3L3}}(0,0,0,0,0,-1,0,0,0)-\text{q1}^2\right),\text{p3}\cdot \;\text{q1}\to \frac{1}{2} \left(-G^{\text{prop3L3}}(0,0,-1,0,0,0,0,0,0)+G^{\text{prop3L3}}(0,0,0,0,0,0,0,-1,0)-\text{q1}^2\right),\text{p4}\cdot \;\text{q1}\to G^{\text{prop3L3}}(0,0,0,0,0,0,0,0,-1)\right\}\right\}$$
