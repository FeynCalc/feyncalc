## FCLoopFindTopologyMappings

`FCLoopFindTopologyMappings[{topo1, topo2, ...}, {p1, p2, ...}]` finds mappings between topologies (written as `FCTopology` objects) `topo1, topo2, ...` that depend on the loop momenta `p1, p2, ...`. For each source topology the function returns a list of loop momentum shits and a `GLI` replacement rule needed to map it to the given target topology. If you need to map everything to a particular set of target topologies, you can specify them via the `PreferredTopologies` option.

### See also

[FCTopology](FCTopology), [GLI](GLI), [FCLoopFindTopologies](FCLoopFindTopologies).

### Examples

```mathematica
topoList = {FCTopology[fctopology1, {SFAD[{{p1 + Q, 0} 
       , {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{
        p1 + p2, 0}, {m1^2, 1}, 1}], SFAD[{{p2 - Q, 0}, {m1^2, 1}, 1}]}], FCTopology[
    fctopology2, {SFAD[{{p1 + p2, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[
      {{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + Q, 0}, {m1^2, 1}, 1}], SFAD[{{p2 - Q, 0}, {m1^2, 
        1}, 1}]}], FCTopology[fctopology3, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 
        0}, {m1^2, 1}, 1}], SFAD[{{p1 + Q, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 
       1}], SFAD[{{p2 - Q, 0}, {m1^2, 1}, 1}]}], FCTopology[fctopology4, {SFAD[{{p2, 0} 
       , {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[
      {{p1 + Q, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 1}]}], FCTopology[
    fctopology5, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[
      {{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - p2 - Q, 0}, {m1^2, 
        1}, 1}]}], FCTopology[fctopology6, {SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1 + Q, 
        0}, {0, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 1}], 
     SFAD[{{p2 - Q, 0}, {m1^2, 1}, 1}]}], FCTopology[fctopology7, {SFAD[{{p2 + Q, 0}, {
        0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{
        p1 - Q, 0}, {m1^2, 1}, 1}]}], FCTopology[fctopology8, {SFAD[{{p2 + Q, 0}, {0, 1}, 1} 
     ], SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {
        0, 1}, 1}]}], FCTopology[fctopology9, {SFAD[{{p1 + Q, 0}, {0, 1}, 1}], SFAD[{{p1 + 
         p2, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology10, {SFAD[{{p1 + p2, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology11, {SFAD[{{p1 + p2, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}]}], 
   FCTopology[fctopology12, {SFAD[{{p1 + p2, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology13, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 
        1}, 1}], SFAD[{{p2 + Q, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology14, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1 - p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology15, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology16, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology17, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology18, {SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 
       1}], SFAD[{{p1 + Q, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology19, {SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 
       1}], SFAD[{{p1 - p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology20, {SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 
       1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - p2 - Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology21, {SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 
       1}], SFAD[{{p1 + p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], 
   FCTopology[fctopology22, {SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 
        1}, 1}], SFAD[{{p1 + p2 + Q, 0}, {m1^2, 1}, 1}]}], FCTopology[fctopology23, {SFAD[
      {{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + p2 + Q, 0}, {0, 1} 
      , 1}]}], FCTopology[fctopology24, {SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {
        m1^2, 1}, 1}], SFAD[{{p1 + p2 + Q, 0}, {0, 1}, 1}]}], FCTopology[fctopology25, {
     SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 + Q, 0}, {m1^2, 
        1}, 1}]}], FCTopology[fctopology26, {SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, 
       {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], FCTopology[fctopology27, {
     SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 
       1}]}], FCTopology[fctopology28, {SFAD[{{p2, 0}, {0, 1}, 1}], SFAD[{{p1, 0}, {0, 
        1}, 1}], SFAD[{{p1 - Q, 0}, {m1^2, 1}, 1}]}], FCTopology[fctopology29, {SFAD[{{
        p1, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {m1^2, 1}, 1}], SFAD[{{p1, 0}, {m1^2, 1}, 1}]}]}
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{((\text{p1}+Q)^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{((\text{p1}+\text{p2})^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology5},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology6},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((\text{p1}+Q)^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology7},\left\{\frac{1}{((\text{p2}+Q)^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology8},\left\{\frac{1}{((\text{p2}+Q)^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology9},\left\{\frac{1}{((\text{p1}+Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology10},\left\{\frac{1}{((\text{p1}+\text{p2})^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology11},\left\{\frac{1}{((\text{p1}+\text{p2})^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology12},\left\{\frac{1}{((\text{p1}+\text{p2})^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology13},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p2}+Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology14},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology15},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology16},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology17},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology18},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology19},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology20},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology21},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology22},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology23},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+Q)^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology24},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+Q)^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology25},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology26},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology27},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology28},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology29},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )}\right\}\right)\right\}$$

```mathematica
FCLoopFindTopologyMappings[topoList, {p1, p2}]
```

$$\left(
\begin{array}{ccc}
 \text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to -\text{p2},\text{p2}\to -\text{p1}\} & G^{\text{fctopology4}}(\text{n3$\_$},\text{n2$\_$},\text{n1$\_$},\text{n5$\_$},\text{n4$\_$}):\to G^{\text{fctopology1}}(\text{n1},\text{n2},\text{n3},\text{n4},\text{n5}) \\
 \text{FCTopology}\left(\text{fctopology14},\left\{\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to Q-\text{p1},\text{p2}\to \text{p2}\} & G^{\text{fctopology14}}(\text{n2$\_$},\text{n4$\_$},\text{n1$\_$},\text{n3$\_$}):\to G^{\text{fctopology10}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \text{FCTopology}\left(\text{fctopology19},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to Q-\text{p1},\text{p2}\to \text{p2}\} & G^{\text{fctopology19}}(\text{n4$\_$},\text{n2$\_$},\text{n1$\_$},\text{n3$\_$}):\to G^{\text{fctopology11}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \text{FCTopology}\left(\text{fctopology9},\left\{\frac{1}{((\text{p1}+Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to -\text{p1},\text{p2}\to -\text{p2}\} & G^{\text{fctopology9}}(\text{n4$\_$},\text{n1$\_$},\text{n2$\_$},\text{n3$\_$}):\to G^{\text{fctopology11}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \text{FCTopology}\left(\text{fctopology7},\left\{\frac{1}{((\text{p2}+Q)^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to -\text{p2},\text{p2}\to -\text{p1}\} & G^{\text{fctopology7}}(\text{n3$\_$},\text{n2$\_$},\text{n1$\_$},\text{n4$\_$}):\to G^{\text{fctopology15}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \text{FCTopology}\left(\text{fctopology20},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-\text{p2}-Q)^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to Q-\text{p1},\text{p2}\to \text{p2}\} & G^{\text{fctopology20}}(\text{n3$\_$},\text{n1$\_$},\text{n2$\_$},\text{n4$\_$}):\to G^{\text{fctopology16}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \text{FCTopology}\left(\text{fctopology21},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}+\text{p2})^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to -\text{p1},\text{p2}\to -\text{p2}\} & G^{\text{fctopology21}}(\text{n1$\_$},\text{n2$\_$},\text{n4$\_$},\text{n3$\_$}):\to G^{\text{fctopology18}}(\text{n1},\text{n2},\text{n3},\text{n4}) \\
 \text{FCTopology}\left(\text{fctopology26},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to -\text{p1},\text{p2}\to -\text{p2}\} & G^{\text{fctopology26}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$}):\to G^{\text{fctopology25}}(\text{n1},\text{n2},\text{n3}) \\
 \text{FCTopology}\left(\text{fctopology28},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2-\text{m1}^2+i \eta )}\right\}\right) & \{\text{p1}\to Q-\text{p1},\text{p2}\to -\text{p2}\} & G^{\text{fctopology28}}(\text{n1$\_$},\text{n3$\_$},\text{n2$\_$}):\to G^{\text{fctopology27}}(\text{n1},\text{n2},\text{n3}) \\
\end{array}
\right)$$