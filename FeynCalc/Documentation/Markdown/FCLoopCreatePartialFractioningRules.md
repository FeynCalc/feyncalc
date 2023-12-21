```mathematica
 
```

## FCLoopCreatePartialFractioningRules

`FCLoopCreatePartialFractioningRules[glis, topos] applies partial fraction decomposition to the given GLIs provided that the corresponding topologies contain linearly dependent propagators. The output is given as a list containing replacement rules and new topologies generated in the course of the decomposition.

### See also

[Overview](Extra/FeynCalc.md), [ApartFF](ApartFF.md), [FCTopology](FCTopology.md), [GLI](GLI.md).

### Examples

```mathematica
glis = {
   GLI["preTopoDia2", {1, 1, 0, 0, 1}], 
   GLI["preTopoDia1", {1, 0, 1, 1, 1}]}
```

$$\left\{G^{\text{preTopoDia2}}(1,1,0,0,1),G^{\text{preTopoDia1}}(1,0,1,1,1)\right\}$$

```mathematica
topos = {FCTopology["preTopoDia1", {SFAD[{{k1, 0}, {0, 1}, 1}], SFAD[{{0, mqb*k1 . nb}, 
       {0, 1}, 1}], SFAD[{{k1, 2*gkin*meta*u0b*k1 . n}, {0, 1}, 1}], 
     SFAD[{{k1, 2*gkin*meta*k1 . n - meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b, 1}, 1}], 
     SFAD[{{k1, 2*gkin*meta*u0b*k1 . n - meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b^2, 1}, 
       1}]}, {k1}, {n, nb}, {Hold[SPD][n] -> 0, Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2}, 
    {}], FCTopology["preTopoDia2", {SFAD[{{k1, 0}, {0, 1}, 1}], SFAD[{{0, -(mqb*k1 . nb)}, {0, 1}, 1}], SFAD[{{k1, meta*u0b*k1 . nb}, {0, 1}, 1}], SFAD[{{k1, -2*gkin*meta*k1 . n + meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b, 1}, 1}], 
       SFAD[{{k1, -2*gkin*meta*u0b*k1 . n + meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b^2, 1}, 
         1}]}, {k1}, {n, nb}, {Hold[SPD][n] -> 0, Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2}, 
    {}]}
```

$$\left\{\text{FCTopology}\left(\text{preTopoDia1},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(\text{mqb} (\text{k1}\cdot \;\text{nb})+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2+i \eta )}\right\},\{\text{k1}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right),\text{FCTopology}\left(\text{preTopoDia2},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(-\text{mqb} (\text{k1}\cdot \;\text{nb})+i \eta )},\frac{1}{(\text{k1}^2+\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})+i \eta )},\frac{1}{(\text{k1}^2+\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{(\text{k1}^2+\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2+i \eta )}\right\},\{\text{k1}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right)\right\}$$

```mathematica
FCLoopCreatePartialFractioningRules[glis, topos]
```

$$\left\{\left\{G^{\text{preTopoDia1}}(1,0,1,1,1)\to -\frac{G^{\text{preTopoDia1PFR12}}(1,1,1)}{2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2}+\frac{G^{\text{preTopoDia1PFR23}}(1,1,1)}{2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2}+\frac{G^{\text{preTopoDia1PFR24}}(1,1,1)}{2 \;\text{gkin} \;\text{meta}^2 (\text{u0b}-1) \;\text{u0b}}-\frac{G^{\text{preTopoDia1PFR25}}(1,1,1)}{2 \;\text{gkin} \;\text{meta}^2 (\text{u0b}-1) \;\text{u0b}}\right\},\left\{\text{FCTopology}\left(\text{preTopoDia1PFR12},\left\{\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2+i \eta )}\right\},\{\text{k1}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right),\text{FCTopology}\left(\text{preTopoDia1PFR23},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2+i \eta )}\right\},\{\text{k1}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right),\text{FCTopology}\left(\text{preTopoDia1PFR25},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}+i \eta )}\right\},\{\text{k1}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right),\text{FCTopology}\left(\text{preTopoDia1PFR24},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)+i \eta )},\frac{1}{(\text{k1}^2+2 \;\text{gkin} \;\text{meta} \;\text{u0b} (\text{k1}\cdot n)-\text{meta} \;\text{u0b} (\text{k1}\cdot \;\text{nb})-2 \;\text{gkin} \;\text{meta}^2 \;\text{u0b}^2+i \eta )}\right\},\{\text{k1}\},\{n,\text{nb}\},\{\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][\text{nb}]\to 0,\text{Hold}[\text{SPD}][n,\text{nb}]\to 2\},\{\}\right)\right\}\right\}$$