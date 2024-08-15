## FCLoopFindSubtopologies

`FCLoopFindSubtopologies[topo]` finds all scalefull subtopologies of the FCTopology `topo`.

Each subtopology receives a marker that specifies the topology from which it was derived. The symbol denoting the marker is specified via the option `SubtopologyMarker`. Setting it to `False` will disable the inclusion of the markers

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),
[SubtopologyMarker](SubtopologyMarker.md).

### Examples

```mathematica
res = FCLoopFindSubtopologies[FCTopology[TRI, {SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1 + Q1, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + Q1, 0}, 
        {0, 1}, 1}], SFAD[{{-p1 + Q2, 0}, {0, 1}, 1}], SFAD[{{-p1 - p2 + Q2, 0}, {0, 1}, 1}]}, 
     {p1, p2}, {Q1, Q2}, {}, {}]];
```

```mathematica
res // Length
```

$$19$$

Show the first three subtopologies of this 2-loop self-energy topology

```mathematica
res[[1 ;; 3]]
```

$$\left\{\text{FCTopology}\left(\text{TRI},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((\text{p1}+\text{Q1})^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{Q1})^2+i \eta )},\frac{1}{((\text{Q2}-\text{p1})^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+\text{Q2})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{\text{Q1},\text{Q2}\},\{\},\{\}\right),\text{FCTopology}\left(\text{TRIR1},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((\text{p1}+\text{Q1})^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{Q1})^2+i \eta )},\frac{1}{((\text{Q2}-\text{p1})^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+\text{Q2})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{\text{Q1},\text{Q2}\},\{\},\{\text{FCGV}(\text{SubtopologyOf})\to \;\text{TRI}\}\right),\text{FCTopology}\left(\text{TRIR2},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}+\text{Q1})^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{Q1})^2+i \eta )},\frac{1}{((\text{Q2}-\text{p1})^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+\text{Q2})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{\text{Q1},\text{Q2}\},\{\},\{\text{FCGV}(\text{SubtopologyOf})\to \;\text{TRI}\}\right)\right\}$$

```mathematica
res = FCLoopFindSubtopologies[FCTopology[topo1, {SFAD[{{p3, 0}, {0, 1}, 1}], 
      SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 1}, 1}], 
      SFAD[{{p2 + p3, 0}, {0, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], 
      SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, 
        {0, 1}, 1}]}, {p1, p2, p3}, {Q}, {}, {}], FCE -> True];
```

```mathematica
res // Length
```

$$36$$

Show the first three subtopologies of this 3-loop self-energy topology

```mathematica
res[[1 ;; 3]]
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo1R1},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\text{FCGV}(\text{SubtopologyOf})\to \;\text{topo1}\}\right),\text{FCTopology}\left(\text{topo1R2},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\text{FCGV}(\text{SubtopologyOf})\to \;\text{topo1}\}\right)\right\}$$