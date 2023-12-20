```mathematica
 
```

## FCLoopFindTopologyMappings

`FCLoopFindTopologyMappings[{topo1, topo2, ...}]` finds mappings between topologies (written as `FCTopology` objects) `topo1, topo2, ...`. For each source topology the function returns a list of loop momentum shifts and a `GLI` replacement rule needed to map it to the given target topology. If you need to map everything to a particular set of target topologies, you can specify them via the `PreferredTopologies` option.

The output is a list of two lists, the former containing the mappings and  the latter enumerating the final contributing topologies

To enable shifts in the external momenta you need to set the option `Momentum` to `All`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopFindTopologies](FCLoopFindTopologies.md).

### Examples

Here we have a set of 5 topologies

```mathematica
topos1 = {
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
     {p1, p2, p3}, {Q}, {}, {}]};
```

3 of them can be mapped to the other two

```mathematica
mappings1 = FCLoopFindTopologyMappings[topos1];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }3\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }2$$

```mathematica
mappings1[[1]]
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to -\text{p1}-\text{p3}+Q,\text{p2}\to -\text{p2}-\text{p3}+Q,\text{p3}\to \;\text{p3}\} & G^{\text{fctopology3}}(\text{n1$\_$},\text{n7$\_$},\text{n8$\_$},\text{n5$\_$},\text{n6$\_$},\text{n4$\_$},\text{n2$\_$},\text{n3$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to Q-\text{p2},\text{p2}\to Q-\text{p1},\text{p3}\to -\text{p3}\} & G^{\text{fctopology4}}(\text{n1$\_$},\text{n6$\_$},\text{n5$\_$},\text{n8$\_$},\text{n7$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n9$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
 \;\text{FCTopology}\left(\text{fctopology5},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right) & \{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1},\text{p3}\to \;\text{p3}\} & G^{\text{fctopology5}}(\text{n1$\_$},\text{n3$\_$},\text{n2$\_$},\text{n4$\_$},\text{n6$\_$},\text{n5$\_$},\text{n7$\_$},\text{n8$\_$},\text{n9$\_$}):\to G^{\text{fctopology2}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7},\text{n8},\text{n9}) \\
\end{array}
\right)$$

And these are the final topologies

```mathematica
mappings1[[2]]
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right)\right\}$$

Here is another example

```mathematica
topos2 = {FCTopology[fctopology1, {SFAD[{{q2, 0}, {0, 1}, 1}], 
     SFAD[{{q1, 0}, {0, 1}, 1}], SFAD[{{q1 + q2, 0}, {0, 1}, 1}], SFAD[{{p + q1, 0}, {0, 1}, 1}], 
     SFAD[{{p - q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
   FCTopology[fctopology2, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1, 0}, {0, 1}, 1}], 
     SFAD[{{p + q2, 0}, {0, 1}, 1}], SFAD[{{p - q1, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
   FCTopology[fctopology3, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1, 0}, {0, 1}, 1}], 
     SFAD[{{p - q1, 0}, {0, 1}, 1}], SFAD[{{p - q1 + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((\text{q1}+\text{q2})^2+i \eta )},\frac{1}{((p+\text{q1})^2+i \eta )},\frac{1}{((p-\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p+\text{q2})^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )},\frac{1}{((p-\text{q1}+\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right)\right\}$$

Yet this time we have some preferred set of topologies and want to match to them (if possible)

```mathematica
preferredTopos2 = {FCTopology[prop2L, {SFAD[{{q1, 0}, {0, 1}, 1}], 
     SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1 - q2, 0}, {0, 1}, 1}], SFAD[{{-p + q1, 0}, {0, 1}, 1}], 
     SFAD[{{-p + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
   FCTopology[prop2LX1, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1 - q2, 0}, {0, 1}, 1}], 
     SFAD[{{-p + q1, 0}, {0, 1}, 1}], SFAD[{{-p + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
   FCTopology[prop2LX3, {SFAD[{{q1, 0}, {0, 1}, 1}], SFAD[{{q2, 0}, {0, 1}, 1}], 
     SFAD[{{-p + q1, 0}, {0, 1}, 1}], SFAD[{{-p + q2, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}], 
   FCTopology[prop2LX15, {SFAD[{{q2, 0}, {0, 1}, 1}], SFAD[{{q1 - q2, 0}, {0, 1}, 1}], 
     SFAD[{{-p + q1, 0}, {0, 1}, 1}]}, {q1, q2}, {p}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{prop2L},\left\{\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{((\text{q1}-\text{q2})^2+i \eta )},\frac{1}{((\text{q1}-p)^2+i \eta )},\frac{1}{((\text{q2}-p)^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{prop2LX1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{((\text{q1}-\text{q2})^2+i \eta )},\frac{1}{((\text{q1}-p)^2+i \eta )},\frac{1}{((\text{q2}-p)^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{prop2LX3},\left\{\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{((\text{q1}-p)^2+i \eta )},\frac{1}{((\text{q2}-p)^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{prop2LX15},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{((\text{q1}-\text{q2})^2+i \eta )},\frac{1}{((\text{q1}-p)^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right)\right\}$$

```mathematica
mappings2 = FCLoopFindTopologyMappings[topos2, PreferredTopologies -> preferredTopos2];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }3\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }3$$

```mathematica
mappings2[[1]]
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((\text{q1}+\text{q2})^2+i \eta )},\frac{1}{((p+\text{q1})^2+i \eta )},\frac{1}{((p-\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right) & \{\text{q1}\to -\text{q2},\text{q2}\to \;\text{q1}\} & G^{\text{fctopology1}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$},\text{n5$\_$},\text{n4$\_$}):\to G^{\text{prop2L}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5}) \\
 \;\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p+\text{q2})^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right) & \{\text{q1}\to \;\text{q2},\text{q2}\to -\text{q1}\} & G^{\text{fctopology2}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$},\text{n4$\_$}):\to G^{\text{prop2LX3}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \;\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )},\frac{1}{((p-\text{q1}+\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right) & \{\text{q1}\to \;\text{q2},\text{q2}\to \;\text{q2}-\text{q1}\} & G^{\text{fctopology3}}(\text{n2$\_$},\text{n1$\_$},\text{n4$\_$},\text{n3$\_$}):\to G^{\text{prop2LX1}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
\end{array}
\right)$$

And these are the final occurring topologies

```mathematica
mappings2[[2]]
```

$$\left\{\text{FCTopology}\left(\text{prop2L},\left\{\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{((\text{q1}-\text{q2})^2+i \eta )},\frac{1}{((\text{q1}-p)^2+i \eta )},\frac{1}{((\text{q2}-p)^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{prop2LX1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{((\text{q1}-\text{q2})^2+i \eta )},\frac{1}{((\text{q1}-p)^2+i \eta )},\frac{1}{((\text{q2}-p)^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{prop2LX3},\left\{\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{((\text{q1}-p)^2+i \eta )},\frac{1}{((\text{q2}-p)^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right)\right\}$$

If we need to match subtopologies into larger topologies, we first need to generate all possible subtopologies for each relevant topology.

```mathematica
topos3 = {
   FCTopology[fctopology1, {
     SFAD[{{l1 + l2 - q1, 0}, {0, 1}, 1}], 
     SFAD[{{l2, 0}, {SMP["m_t"]^2, 1}, 1}], 
     SFAD[{{l1, 0}, {SMP["m_t"]^2, 1}, 1}], 
     SFAD[{{l2 + q2, 0}, {SMP["m_t"]^2, 1}, 1}], 
     SFAD[{{l1 - q1, 0}, {SMP["m_t"]^2, 1}, 1}], 
     SFAD[{{l1 - q1 - q2, 0}, {SMP["m_t"]^2, 1}, 1}]}, {l1, l2}, {q1, q2}, {}, {}], 
   FCTopology[fctopology9, {
     SFAD[{{l1 + l2 + q2, 0}, {0, 1}, 1}], 
     SFAD[{{l2, 0}, {SMP["m_t"]^2, 1}, 1}], 
     SFAD[{{l1, 0}, {SMP["m_t"]^2, 1}, 1}], 
     SFAD[{{l1 + q2, 0}, {SMP["m_t"]^2, 1}, 1}], 
     SFAD[{{l1 - q1, 0}, {SMP["m_t"]^2, 1}, 1}]}, {l1, l2}, {q1, q2}, {}, {}] 
   }
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{((\text{l1}+\text{l2}-\text{q1})^2+i \eta )},\frac{1}{(\text{l2}^2-m_t^2+i \eta )},\frac{1}{(\text{l1}^2-m_t^2+i \eta )},\frac{1}{((\text{l2}+\text{q2})^2-m_t^2+i \eta )},\frac{1}{((\text{l1}-\text{q1})^2-m_t^2+i \eta )},\frac{1}{((\text{l1}-\text{q1}-\text{q2})^2-m_t^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology9},\left\{\frac{1}{((\text{l1}+\text{l2}+\text{q2})^2+i \eta )},\frac{1}{(\text{l2}^2-m_t^2+i \eta )},\frac{1}{(\text{l1}^2-m_t^2+i \eta )},\frac{1}{((\text{l1}+\text{q2})^2-m_t^2+i \eta )},\frac{1}{((\text{l1}-\text{q1})^2-m_t^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\{\},\{\}\right)\right\}$$

```mathematica
subTopos3 = Flatten[FCLoopFindSubtopologies[topos3]];
```

```mathematica
subTopos3 // Length
```

$$37$$

Now we can match a smaller topology into a larger topology

```mathematica
mappings3 = FCLoopFindTopologyMappings[topos3, PreferredTopologies -> subTopos3];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }1\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

```mathematica
mappings3[[1]]
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{fctopology9},\left\{\frac{1}{((\text{l1}+\text{l2}+\text{q2})^2+i \eta )},\frac{1}{(\text{l2}^2-m_t^2+i \eta )},\frac{1}{(\text{l1}^2-m_t^2+i \eta )},\frac{1}{((\text{l1}+\text{q2})^2-m_t^2+i \eta )},\frac{1}{((\text{l1}-\text{q1})^2-m_t^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\{\},\{\}\right) & \{\text{l1}\to \;\text{q1}-\text{l1},\text{l2}\to -\text{l2}-\text{q2}\} & G^{\text{fctopology9}}(\text{n1$\_$},\text{n3$\_$},\text{n4$\_$},\text{n5$\_$},\text{n2$\_$}):\to G^{\text{fctopology1}}(\text{n1},0,\text{n2},\text{n3},\text{n4},\text{n5}) \\
\end{array}
\right)$$

```mathematica
mappings3[[2]]
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{((\text{l1}+\text{l2}-\text{q1})^2+i \eta )},\frac{1}{(\text{l2}^2-m_t^2+i \eta )},\frac{1}{(\text{l1}^2-m_t^2+i \eta )},\frac{1}{((\text{l2}+\text{q2})^2-m_t^2+i \eta )},\frac{1}{((\text{l1}-\text{q1})^2-m_t^2+i \eta )},\frac{1}{((\text{l1}-\text{q1}-\text{q2})^2-m_t^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\{\},\{\}\right)\right\}$$

Mapping the following two topologies onto each other requires shifts in the external momenta due to the chosen kinematic constraints.

```mathematica
topos4 = {
   FCTopology[topo1, {
     SFAD[{{l1 + q1, 0}, {m^2, 1}, 1}], 
     SFAD[{{l1 - l2, 0}, {0, 1}, 1}], 
     SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
     SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
     SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, {SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}], 
   FCTopology[topo2, {
     SFAD[{{l1 - l2, 0}, {m^2, 1}, 1}], 
     SFAD[{{l1 - q2, 0}, {0, 1}, 1}], 
     SFAD[{{l2 - q2, 0}, {m^2, 1}, 1}], 
     SFAD[{{l2 + q1, 0}, {m^2, 1}, 1}], 
     SFAD[{{l2, 0}, {0, 1}, 1}]}, {l1, l2}, {q1, q2}, {SPD[q1, q1] -> 0, SPD[q2, q2] -> 0, SPD[q1, q2] -> s/2}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{((\text{l1}+\text{q1})^2-m^2+i \eta )},\frac{1}{((\text{l1}-\text{l2})^2+i \eta )},\frac{1}{((\text{l2}+\text{q1})^2-m^2+i \eta )},\frac{1}{((\text{l2}-\text{q2})^2-m^2+i \eta )},\frac{1}{(\text{l2}^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\left\{\text{q1}^2\to 0,\text{q2}^2\to 0,\text{q1}\cdot \;\text{q2}\to \frac{s}{2}\right\},\{\}\right),\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{((\text{l1}-\text{l2})^2-m^2+i \eta )},\frac{1}{((\text{l1}-\text{q2})^2+i \eta )},\frac{1}{((\text{l2}-\text{q2})^2-m^2+i \eta )},\frac{1}{((\text{l2}+\text{q1})^2-m^2+i \eta )},\frac{1}{(\text{l2}^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\left\{\text{q1}^2\to 0,\text{q2}^2\to 0,\text{q1}\cdot \;\text{q2}\to \frac{s}{2}\right\},\{\}\right)\right\}$$

```mathematica
mappings4 = FCLoopFindTopologyMappings[topos4, Momentum -> All];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }1\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

```mathematica
mappings4[[1]]
```

$$\left(
\begin{array}{ccc}
 \;\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{((\text{l1}-\text{l2})^2-m^2+i \eta )},\frac{1}{((\text{l1}-\text{q2})^2+i \eta )},\frac{1}{((\text{l2}-\text{q2})^2-m^2+i \eta )},\frac{1}{((\text{l2}+\text{q1})^2-m^2+i \eta )},\frac{1}{(\text{l2}^2+i \eta )}\right\},\{\text{l1},\text{l2}\},\{\text{q1},\text{q2}\},\left\{\text{q1}^2\to 0,\text{q2}^2\to 0,\text{q1}\cdot \;\text{q2}\to \frac{s}{2}\right\},\{\}\right) & \{\text{l1}\to -\text{l1}+\text{l2}-\text{q1},\text{l2}\to \;\text{l2},\text{q1}\to -\text{q2},\text{q2}\to -\text{q1}\} & G^{\text{topo2}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$},\text{n4$\_$},\text{n5$\_$}):\to G^{\text{topo1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5}) \\
\end{array}
\right)$$

Otherwise no mappings exist

```mathematica
FCLoopFindTopologyMappings[topos4][[1]]
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }2$$

$$\{\}$$

Topologies containing eikonal or other nonstandard propagators may introduce additional challenges.
Even though two such topologies can be recognized to be identical, the code still would not be able to
work out the correct momentum shifts without some additional input.

```mathematica
topoEik1 = FCTopology[mytopo67, {SFAD[{{k2, 0}, {0, 1}, 1}], SFAD[{{k1, 0}, {0, 1}, 1}], 
     SFAD[{{k1 + k2, 0}, {0, 1}, 1}], SFAD[{{0, -k1 . nb}, {0, 1}, 1}], 
     SFAD[{{k2, -meta u0b k2 . nb}, {0, 1}, 1}], SFAD[{{k1 + k2, -2 gkin meta u0b (k1 + k2) . n}, 
       {0, 1}, 1}], SFAD[{{k1, -2 gkin meta k1 . n + meta u0b k1 . nb}, {2 gkin meta^2 u0b, 1}, 1}]}, 
    {k1, k2}, {n, nb}, {Hold[SPD][n] -> 0, Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2}, {}];
```

```mathematica
topoEik2 = FCTopology[mytopo79, {SFAD[{{k2, 0}, {0, 1}, 1}], SFAD[{{k1, 0}, {0, 1}, 1}], 
     SFAD[{{0, k1 . nb}, {0, 1}, 1}], SFAD[{{k2, -meta u0b k2 . nb}, {0, 1}, 1}], 
     SFAD[{{k1 + k2, -meta u0b (k1 + k2) . nb}, {0, 1}, 1}], SFAD[{{k1, 
        2 gkin meta k1 . n - meta u0b k1 . nb}, {2 gkin meta^2 u0b, 1}, 1}], 
     SFAD[{{k1 + k2, 2 gkin meta u0b (k1 + k2) . n - meta u0b (k1 + k2) . nb}, 
       {2 gkin meta^2 u0b^2, 1}, 1}]}, {k1, k2}, {n, nb}, {Hold[SPD][n] -> 0, 
     Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2}, {}];
```

```mathematica
DataType[meta, FCVariable] = True;
DataType[u0b, FCVariable] = True;
```

At first sight these two topologies are independent from each other

```mathematica
FCLoopFindTopologyMappings[{topoEik1, topoEik2}];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }2$$

However, if we tell the code how some eikonal propagators can be brought into a quadratic form,
then an explicit mapping can be found

```mathematica
eikRule = {SFAD[{{k2, -meta u0b k2 . nb}, {0, 1}, 1}] -> SFAD[k2 - meta u0b/2 nb]}
```

$$\left\{\frac{1}{(\text{k2}^2-\text{meta} \;\text{u0b} (\text{k2}\cdot \;\text{nb})+i \eta )}\to \frac{1}{((\text{k2}-\frac{\text{meta} \;\text{u0b} \;\text{nb}}{2})^2+i \eta )}\right\}$$

```mathematica
eikMappings = FCLoopFindTopologyMappings[{topoEik1, topoEik2}, 
    InitialSubstitutions -> eikRule];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }1\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

```mathematica
eikMappings[[1]][[1]][[2 ;;]]
```

$$\left\{\left\{\text{k1}\to -\text{k1},\text{k2}\to \frac{1}{2} (\text{meta} \;\text{nb} \;\text{u0b}-2 \;\text{k2})\right\},G^{\text{mytopo79}}(\text{n5$\_$},\text{n2$\_$},\text{n4$\_$},\text{n1$\_$},\text{n3$\_$},\text{n7$\_$},\text{n6$\_$}):\to G^{\text{mytopo67}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5},\text{n6},\text{n7})\right\}$$