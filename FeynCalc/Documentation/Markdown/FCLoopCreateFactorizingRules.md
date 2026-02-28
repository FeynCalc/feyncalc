## FCLoopCreateFactorizingRules

`FCLoopCreateFactorizingRules[ints, topos]` processes the given list of GLIs and corresponding topologies and returns a list of rules for replacing all factorizing integrals by simpler integrals with less loops.

Notice that we automatically generate suitable `FCTopology` objects for the simpler integrals. Using the options `PreferredTopologies` or `PreferredIntegrals` those can be mapped to a desired set.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFactorizingQ](FCLoopFactorizingQ.md), [FCLoopCreateFactorizingRules](FCLoopCreateFactorizingRules.md).

### Examples

```mathematica
masters = Get@FileNameJoin[{$FeynCalcDirectory, "Documentation", "Examples", "MasterIntegrals", 
      "mastersBMixing3L.m"}];
```

```mathematica
topos = Get@FileNameJoin[{$FeynCalcDirectory, "Documentation", "Examples", "MasterIntegrals", 
      "toposBMixing3L.m"}];
```

```mathematica
FCLoopCreateFactorizingRules[masters[[1 ;; 50]], topos]

```mathematica

$$\text{FCLoopCreateFactorizingRules: }\;\text{Number of factorizing integrals: }11$$

$$\text{FCLoopCreateFactorizingRules: }\;\text{Number of simpler integrals: }6$$

$$\left\{\left\{G^{\text{prop2Ltopo00011}}(0,0,0,1,1)\to G^{\text{loopint1}}(1)^2,G^{\text{prop2Ltopo00011}}(1,1,0,0,1)\to G^{\text{loopint1}}(1) G^{\text{loopint7}}(1,1),G^{\text{prop2Ltopo00110}}(1,1,1,1,0)\to G^{\text{loopint11}}(1,1) G^{\text{loopint7}}(1,1),G^{\text{prop2Ltopo00111}}(0,0,1,1,1)\to G^{\text{loopint1}}(1) G^{\text{loopint11}}(1,1),G^{\text{prop2Ltopo00303}}(0,0,1,0,1)\to G^{\text{loopint3}}(1)^2,G^{\text{prop2Ltopo01013}}(0,0,0,1,1)\to G^{\text{loopint1}}(1) G^{\text{loopint3}}(1),G^{\text{prop2Ltopo01310}}(0,1,1,1,0)\to G^{\text{loopint1}}(1) G^{\text{loopint12}}(1,1),G^{\text{prop2Ltopo01313}}(0,0,1,1,1)\to G^{\text{loopint3}}(1) G^{\text{loopint12}}(1,1),G^{\text{prop2Ltopo02020}}(0,1,0,1,0)\to G^{\text{loopint13}}(1)^2,G^{\text{prop2Ltopo02023}}(0,0,0,1,1)\to G^{\text{loopint13}}(1) G^{\text{loopint3}}(1),G^{\text{prop2Ltopo02102}}(0,0,1,0,1)\to G^{\text{loopint1}}(1) G^{\text{loopint13}}(1)\right\},\left\{G^{\text{loopint1}}(1),G^{\text{loopint11}}(1,1),G^{\text{loopint12}}(1,1),G^{\text{loopint13}}(1),G^{\text{loopint3}}(1),G^{\text{loopint7}}(1,1)\right\},\left\{\text{FCTopology}\left(\text{loopint1},\left\{\frac{1}{(-\text{p1}^2+\text{m1}^2+i \eta )}\right\},\{\text{p1}\},\{\},\{\},\{\}\right),\text{FCTopology}\left(\text{loopint11},\left\{\frac{1}{(-\text{p3}^2+\text{m1}^2+i \eta )},\frac{1}{(-(\text{p3}+\text{q1})^2+\text{m1}^2+i \eta )}\right\},\{\text{p3}\},\{\text{q1}\},\{\},\{\}\right),\text{FCTopology}\left(\text{loopint12},\left\{\frac{1}{(-\text{p3}^2+\text{m3}^2+i \eta )},\frac{1}{(-(\text{p3}+\text{q1})^2+\text{m1}^2+i \eta )}\right\},\{\text{p3}\},\{\text{q1}\},\{\},\{\}\right),\text{FCTopology}\left(\text{loopint13},\left\{\frac{1}{(-(\text{p3}+\text{q1})^2+\text{m2}^2+i \eta )}\right\},\{\text{p3}\},\{\text{q1}\},\{\},\{\}\right),\text{FCTopology}\left(\text{loopint3},\left\{\frac{1}{(-\text{p1}^2+\text{m3}^2+i \eta )}\right\},\{\text{p1}\},\{\},\{\},\{\}\right),\text{FCTopology}\left(\text{loopint7},\left\{\frac{1}{(-\text{p1}^2+i \eta )},\frac{1}{(-(\text{p1}+\text{q1})^2+i \eta )}\right\},\{\text{p1}\},\{\text{q1}\},\{\},\{\}\right)\right\}\right\}$$