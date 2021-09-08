`FCLoopFindTopologyMappings[{topo1, topo2, ...}]` finds mappings between topologies (written as `FCTopology` objects) `topo1, topo2, ...`. For each source topology the function returns a list of loop momentum shifts and a `GLI` replacement rule needed to map it to the given target topology. If you need to map everything to a particular set of target topologies, you can specify them via the `PreferredTopologies` option.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopFindTopologies](FCLoopFindTopologies.md).

### Examples

```mathematica
FCLoopFindTopologyMappings[{
   FCTopology[fctopology1, {SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
     SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
   FCTopology[fctopology2, {SFAD[{{p3, 0}, {0, 1}, 1}], 
     SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
     SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], 
   FCTopology[fctopology3, {SFAD[{{p3, 0}, {0, 1}, 1}], 
     SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
     SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}],
     SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, 
    {p1, p2, p3}, {Q}, {}, {}], 
   FCTopology[fctopology4, {SFAD[{{p3, 0}, {0, 1}, 1}], 
     SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
     SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p3, 0}, {0, 1}, 1}],
     SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, 
    {p1, p2, p3}, {Q}, {}, {}], 
   FCTopology[fctopology5, {SFAD[{{p3, 0}, {0, 1}, 1}], 
     SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], 
     SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]}, 
    {p1, p2, p3}, {Q}, {}, {}]}]
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q,\text{p3}\to \;\text{p3}\} & G^{\text{fctopology3}}(\text{n1$\_$},\text{n7$\_$},\text{n8$\_$},\text{n5$\_$},\text{n6$\_$},\text{n4$\_$},\text{n2$\_$},\text{n3$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\} & G^{\text{fctopology4}}(\text{n1$\_$},\text{n6$\_$},\text{n5$\_$},\text{n8$\_$},\text{n7$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology5},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1},\text{p3}\to \;\text{p3}\} & G^{\text{fctopology5}}(\text{n1$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n6$\_$},\text{n5$\_$},\text{n7$\_$},\text{n8$\_$},\text{n9$\_$}):\to G^{\text{fctopology2}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
\end{array}
\right)$$

```mathematica
FCLoopFindTopologyMappings[{FCTopology[fctopology1, {SFAD[{{q2, 0}, {0, 1}, 1}], 
     SFAD[{{q1, 0}, {0, 1}, 1}], SFAD[{{q1 + q2, 0}, {0, 1}, 1}], SFAD[{{p + q1, 0}, {0, 1}, 1}], 
     SFAD[{{p - q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
   FCTopology[fctopology2, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1, 0}, {0, 1}, 1}], 
     SFAD[{{p + q2, 0}, {0, 1}, 1}], SFAD[{{p - q1, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
   FCTopology[fctopology3, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1, 0}, {0, 1}, 1}], 
     SFAD[{{p - q1, 0}, {0, 1}, 1}], SFAD[{{p - q1 + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}]}, 
  PreferredTopologies -> {FCTopology[prop2L, {SFAD[{{q1, 0}, {0, 1}, 1}], 
      SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1 - q2, 0}, {0, 1}, 1}], SFAD[{{-p + q1, 0}, {0, 1}, 1}], 
      SFAD[{{-p + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
    FCTopology[prop2LX1, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1 - q2, 0}, {0, 1}, 1}], 
      SFAD[{{-p + q1, 0}, {0, 1}, 1}], SFAD[{{-p + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
    FCTopology[prop2LX3, {SFAD[{{q1, 0}, {0, 1}, 1}], SFAD[{{q2, 0}, {0, 1}, 1}],
      SFAD[{{-p + q1, 0}, {0, 1}, 1}], SFAD[{{-p + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
    FCTopology[prop2LX15, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1 - q2, 0}, {0, 1}, 1}], 
      SFAD[{{-p + q1, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}]}]
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((\text{q1}+\text{q2})^2+i \eta )},\frac{1}{((p+\text{q1})^2+i \eta )},\frac{1}{((p-\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right) & \{\text{q1}\to -\text{q2},\text{q2}\to \;\text{q1}\} & G^{\text{fctopology1}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$},\text{n5$\_$},\text{n4$\_$}):\to G^{\text{prop2L}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5}) \\
 \;\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p+\text{q2})^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right) & \{\text{q1}\to \;\text{q2},\text{q2}\to -\text{q1}\} & G^{\text{fctopology2}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$},\text{n4$\_$}):\to G^{\text{prop2LX3}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )},\frac{1}{((p-\text{q1}+\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right) & \{\text{q1}\to \;\text{q2},\text{q2}\to \;\text{q2}-\text{q1}\} & G^{\text{fctopology3}}(\text{n2$\_$},\text{n1$\_$},\text{n4$\_$},\text{n3$\_$}):\to G^{\text{prop2LX1}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
\end{array}
\right)$$
