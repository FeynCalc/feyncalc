## FCLoopAddScalingParameter

`FCLoopAddScalingParameter[topo, la, rules]` multiplies masses and momenta in the propagators of the topology `topo` by the scaling parameter `la` according to the scaling rules in `rules`. The `id` of the topology remains unchanged. This is useful e.g. for asymptotic expansions of the corresponding loop integrals
given as GLIs.

The scaling variable should be declared as `FCVariable` via the `DataType` mechanism.

Notice that if all terms in a propagator have the same scaling, the scaling variable in the respective propagator will be set to unity.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).

### Examples

```mathematica
DataType[la, FCVariable] = True;
```

We declare the external 4-momentum `q` as our hard scale, while the mass `mc` is soft

```mathematica
topoScaled = FCLoopAddScalingParameter[FCTopology[prop1LtopoC11, {SFAD[{{I p1, 0}, {-mc^2, -1}, 1}], 
     SFAD[{{I (p1 - q), 0}, {-mc^2, -1}, 1}]}, {p1}, {q}, {SPD[q, q] ->mb^2}, {}], la, 
   {q -> la^0 q, mc -> la^1 mc}]
```

$$\text{Scalings of momenta and masses in the propagators of }\;\text{prop1LtopoC11}\;\text{ : }\left\{\text{mb}\to \;\text{la}^0 \;\text{mb},\text{mc}\to \;\text{la} \;\text{mc},\text{p1}\to \;\text{la}^0 \;\text{p1},q\to \;\text{la}^0 q\right\}$$

$$\text{FCTopology}\left(\text{prop1LtopoC11},\left\{\frac{1}{(\text{la}^2 \;\text{mc}^2-\text{p1}^2-i \eta )},\frac{1}{(-\text{mb}^2+\text{la}^2 \;\text{mc}^2-\text{p1}^2+2 (\text{p1}\cdot q)-i \eta )}\right\},\{\text{p1}\},\{q\},\left\{q^2\to \;\text{mb}^2\right\},\{\}\right)$$

Having set up the scaling we can now use `FCLoopGLIExpand` to expand the loop integrals belonging to this topology up to
the desired order in `la`. Here we choose $\mathcal{O}(\lambda^4)$

```mathematica
FCLoopGLIExpand[GLI[prop1LtopoC11, {1, 1}], {topoScaled}, {la, 0, 4}]
```

$$\left\{\text{la}^4 \;\text{mc}^4 G^{\text{prop1LtopoC11}}(1,3)+\text{la}^4 \;\text{mc}^4 G^{\text{prop1LtopoC11}}(2,2)+\text{la}^4 \;\text{mc}^4 G^{\text{prop1LtopoC11}}(3,1)-\text{la}^2 \;\text{mc}^2 G^{\text{prop1LtopoC11}}(1,2)-\text{la}^2 \;\text{mc}^2 G^{\text{prop1LtopoC11}}(2,1)+G^{\text{prop1LtopoC11}}(1,1),\left\{\text{FCTopology}\left(\text{prop1LtopoC11},\left\{\frac{1}{(-\text{p1}^2-i \eta )},\frac{1}{(-\text{mb}^2-\text{p1}^2+2 (\text{p1}\cdot q)-i \eta )}\right\},\{\text{p1}\},\{q\},\left\{q^2\to \;\text{mb}^2\right\},\{\}\right)\right\}\right\}$$