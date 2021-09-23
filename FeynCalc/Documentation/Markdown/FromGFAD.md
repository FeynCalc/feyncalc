## FromGFAD

`FromGFAD[exp]` converts all suitable generic propagator denominators into standard and Cartesian propagator denominators.

The options `InitialSubstitutions` and `IntermediateSubstitutions` can be used to help the function handle nontrivial propagators.

For propagators containing symbolic variables it might be necessary to tell the function that those are larger than zero (if applicable), so that expressions such as $\sqrt{\lambda^2}$ can be simplified accordingly.

### See also

[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md).

### Examples

```mathematica
GFAD[SPD[p1]]
FromGFAD[%]
% // StandardForm
```

$$\frac{1}{(\text{p1}^2+i \eta )}$$

$$\frac{1}{(\text{p1}^2+i \eta )}$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p1, D], 0, 0, {1, 1}]]*)
```

```mathematica
GFAD[SPD[p1] + 2 SPD[p1, p2]]
FromGFAD[%]
% // StandardForm
```

$$\frac{1}{(\text{p1}^2+2 (\text{p1}\cdot \;\text{p2})+i \eta )}$$

$$\frac{1}{(\text{p1}^2+2 (\text{p1}\cdot \;\text{p2})+i \eta )}$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p1, D], 2 Pair[Momentum[p1, D], Momentum[p2, D]], 0, {1, 1}]]*)
```

```mathematica
GFAD[{{CSPD[p1] + 2 CSPD[p1, p2] + m^2, -1}, 2}]
FromGFAD[%]
% // StandardForm
```

$$\frac{1}{(m^2+\text{p1}^2+2 (\text{p1}\cdot \;\text{p2})-i \eta )^2}$$

$$\frac{1}{(\text{p1}^2+2 (\text{p1}\cdot \;\text{p2})+m^2-i \eta )^2}$$

```
(*FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p1, -1 + D], 2 CartesianPair[CartesianMomentum[p1, -1 + D], CartesianMomentum[p2, -1 + D]], m^2, {2, -1}]]*)
```

```mathematica
prop = FeynAmpDenominator[GenericPropagatorDenominator[-la Pair[Momentum[p1, D], Momentum[p1, D]] + 2 Pair[Momentum[p1, D], Momentum[q, D]], {1, 1}]]
```

$$\frac{1}{(2 (\text{p1}\cdot q)-\text{la} \;\text{p1}^2+i \eta )}$$

```mathematica
FromGFAD[prop]
% // StandardForm
```

$$\frac{1}{(-\text{la} \;\text{p1}^2+2 (\text{p1}\cdot q)+i \eta )}$$

$$\text{FeynAmpDenominator}\left[\text{StandardPropagatorDenominator}\left[\sqrt{-\text{la}} \;\text{Momentum}[\text{p1},D],2 \;\text{Pair}[\text{Momentum}[\text{p1},D],\text{Momentum}[q,D]],0,\{1,1\}\right]\right]$$

```mathematica
FromGFAD[prop, PowerExpand -> {la}]
% // StandardForm
```

$$\frac{1}{(-\text{la} \;\text{p1}^2+2 (\text{p1}\cdot q)+i \eta )}$$

$$\text{FeynAmpDenominator}\left[\text{StandardPropagatorDenominator}\left[i \sqrt{\text{la}} \;\text{Momentum}[\text{p1},D],2 \;\text{Pair}[\text{Momentum}[\text{p1},D],\text{Momentum}[q,D]],0,\{1,1\}\right]\right]$$

```mathematica
ex = GFAD[{{-SPD[p1, p1], 1}, 1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*GFAD[{{-SPD[p3, p3], 1}, 1}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] + 
   (-2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 2}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*GFAD[{{-SPD[p3, p3], 1}, 1}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] - 
        2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 2}]*GFAD[{{-SPD[p3, p3], 1}, 1}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] - 
        2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*GFAD[{{-SPD[p3, p3], 1}, 2}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}])/2
```

$$\frac{1}{2} \left(-\frac{2 \;\text{mg}^2}{(-\text{p1}^2+i \eta )^2 (-\text{p3}^2+i \eta ) (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (\text{p1}\cdot (2 \;\text{p3}-\text{p1})-\text{p3}^2+i \eta )}-\frac{2 \;\text{mg}^2}{(-\text{p1}^2+i \eta ) (-\text{p3}^2+i \eta ) (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (\text{p1}\cdot (2 \;\text{p3}-\text{p1})-\text{p3}^2+i \eta )^2}-\frac{2 \;\text{mg}^2}{(-\text{p1}^2+i \eta ) (-\text{p3}^2+i \eta )^2 (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (\text{p1}\cdot (2 \;\text{p3}-\text{p1})-\text{p3}^2+i \eta )}\right)+\frac{1}{(-\text{p1}^2+i \eta ) (-\text{p3}^2+i \eta ) (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (\text{p1}\cdot (2 \;\text{p3}-\text{p1})-\text{p3}^2+i \eta )}$$

```mathematica
FromGFAD[ex]
```

$$\frac{1}{2} \left(-\frac{2 \;\text{mg}^2}{(-\text{p1}^2+i \eta )^2 (-\text{p3}^2+i \eta ) (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (-\text{p3}^2+\text{p1}\cdot (2 \;\text{p3}-\text{p1})+i \eta )}-\frac{2 \;\text{mg}^2}{(-\text{p1}^2+i \eta ) (-\text{p3}^2+i \eta )^2 (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (-\text{p3}^2+\text{p1}\cdot (2 \;\text{p3}-\text{p1})+i \eta )}-\frac{2 \;\text{mg}^2}{(-\text{p1}^2+i \eta ) (-\text{p3}^2+i \eta ) (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (-\text{p3}^2+\text{p1}\cdot (2 \;\text{p3}-\text{p1})+i \eta )^2}\right)+\frac{1}{(-\text{p1}^2+i \eta ) (-\text{p3}^2+i \eta ) (-(\text{p1}+q)^2+\text{mb}^2+i \eta ) (-(\text{p3}+q)^2+\text{mb}^2+i \eta ) (-\text{p3}^2+\text{p1}\cdot (2 \;\text{p3}-\text{p1})+i \eta )}$$
