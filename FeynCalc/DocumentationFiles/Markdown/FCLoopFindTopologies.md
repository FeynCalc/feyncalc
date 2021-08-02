`FCLoopFindTopologies[exp, {q1, q2, ...}]` attempts to identify the loop integral topologies present in `exp` by looking at the propagator denominators that depend on the loop momenta `q1, q2, ...` . It returns a list of two entries, where the first one is the original expression with the denominators rewritten as `GLI`s, and the second one is the set of the identified topologies. Each of the identified topologies must contain linearly independent propagators (unless the option `FCLoopBasisOverdeterminedQ` is set to True), but may lack propagators needed to form a complete basis.

### See also

[FCTopology](FCTopology), [GLI](GLI).

### Examples

```mathematica
FCLoopFindTopologies[GFAD[{{-SPD[p1, p2] + SPD[p1, Q]*SPD[p2, Q], 1}, 1}] 
   *HoldForm[cc1]*SFAD[{{p1, 0}, {0, 1}, 1}]*SFAD[{{p2, 0}, {0, 1}, 1}]*SFAD[{{
       p3, 0}, {0, 1}, 1}]*SFAD[{{p1 - Q, 0}, {0, 1}, 2}]*SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}]*
    SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}] + GFAD[{{-SPD[p1, p2] + SPD[p1, Q]*SPD[p2, Q], 
       1}, 1}]*HoldForm[cc2]*SFAD[{{p1, 0}, {0, 1}, 1}]*SFAD[{{p2, 0}, {0, 1}, 1}]*
    SFAD[{{p3, 0}, {0, 1}, 1}]*SFAD[{{p1 - Q, 0}, {0, 1}, 1}]*SFAD[{{p1 + p2 - Q, 0}, {0, 
       1}, 1}]*SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}]*SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}] + 
   GFAD[{{-SPD[p1, p2] + SPD[p1, Q]*SPD[p2, Q], 1}, 1}]*HoldForm[cc3]*SFAD[{{p1, 
       0}, {0, 1}, 1}]*SFAD[{{p2, 0}, {0, 1}, 1}]*SFAD[{{p3, 0}, {0, 1}, 1}]*SFAD[{{p1 - 
        Q, 0}, {0, 1}, 1}]*SFAD[{{p2 - Q, 0}, {0, 1}, 1}]*SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}]*
    SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}] + GFAD[{{-SPD[p1, p2] + SPD[p1, Q]*SPD[p2, Q], 
       1}, 1}]*HoldForm[cc4]*SFAD[{{p1, 0}, {0, 1}, 1}]*SFAD[{{p2, 0}, {0, 1}, 1}]*
    SFAD[{{p3, 0}, {0, 1}, 1}]*SFAD[{{p2 + p3, 0}, {0, 1}, 1}]*SFAD[{{p1 - Q, 0}, {0, 1}, 
      1}]*SFAD[{{p1 + p2 - Q, 0}, {0, 1}, 1}]*SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}]*SFAD[{{p1 + 
        p2 + p3 - Q, 0}, {0, 1}, 1}] + cc5*GFAD[{{-SPD[p1, p2] + SPD[p1, Q]*SPD[p2, Q], 1}, 1}] 
   *SFAD[{{p1, 0}, {0, 1}, 1}]*SFAD[{{p2, 0}, {0, 1}, 1}]*SFAD[{{p3, 0}, {0, 1}, 1}]*
    SFAD[{{p2 - Q, 0}, {0, 1}, 1}]*SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}]*SFAD[{{p2 + p3 - Q, 0} 
      , {0, 1}, 1}]*SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], {p1, p2,p3}, FCPrint -> 
   False, FCE -> True]
```

$$\left\{\text{FCGV}(\text{GLIProduct})\left(\text{cc1},G^{\text{fctopology1}}(1,1,1,0,2,0,1,1,1)\right)+\text{FCGV}(\text{GLIProduct})\left(\text{cc2},G^{\text{fctopology4}}(1,1,1,1,1,1,1,1)\right)+\text{FCGV}(\text{GLIProduct})\left(\text{cc3},G^{\text{fctopology3}}(1,1,1,1,1,1,1,1)\right)+\text{FCGV}(\text{GLIProduct})\left(\text{cc4},G^{\text{fctopology1}}(1,1,1,1,1,1,1,1,1)\right)+\text{FCGV}(\text{GLIProduct})\left(\text{cc5},G^{\text{fctopology2}}(1,1,1,1,1,1,1,1)\right),\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}\cdot Q) (\text{p2}\cdot Q)-\text{p1}\cdot \text{p2}+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}\cdot Q) (\text{p2}\cdot Q)-\text{p1}\cdot \text{p2}+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}\cdot Q) (\text{p2}\cdot Q)-\text{p1}\cdot \text{p2}+i \eta )}\right\}\right),\text{FCTopology}\left(\text{fctopology4},\left\{\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{((\text{p1}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}-Q)^2+i \eta )},\frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )},\frac{1}{((\text{p1}\cdot Q) (\text{p2}\cdot Q)-\text{p1}\cdot \text{p2}+i \eta )}\right\}\right)\right\}\right\}$$